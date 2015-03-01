{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell #-}
module Main where

import Language.Subleq.Model.Prim
import Language.Subleq.Model.Memory as Mem
import Language.Subleq.Model.Architecture.IntMachine
import qualified Language.Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Language.Subleq.Assembly as A
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint hiding ((<+>))
import qualified Text.PrettyPrint as PP
import qualified Data.ByteString as B
-- import Data.Maybe
-- import Data.Map (Map)
import qualified Data.Map as M
import Data.FileEmbed
-- import Control.Monad.State
-- import Control.Lens
import Control.Arrow
import Test.QuickCheck
-- import Test.QuickCheck.Text
-- import Test.QuickCheck.All
import Data.Word
import Data.Int
import Data.Bits
import Data.Function
import Text.Printf

type SubleqWord = Int32
type SubleqUWord = Word32
wordLength :: (Integral a, Num a) => a
wordLength = 32

type Fix2SubleqMemory = M.Map SubleqWord SubleqWord
type Fix2SubleqState = SubleqState SubleqWord SubleqWord Fix2SubleqMemory

locateArg :: A.LocateArg
locateArg xs = M.fromList $ zip xs [0..3]

subleqRoutines :: B.ByteString
subleqRoutines = $(embedFile "subleq-int-fix2.sq")

subleqModule :: A.Module
subleqModule = either (error . show) A.expandMacroAll $ parse A.parseModule "parserModule" subleqRoutines

inc, dec :: SubleqWord
inc = 0x4
dec = 0x5

subleqMA :: A.MemoryArchitecture (M.Map SubleqWord SubleqWord)
subleqMA = A.MemoryArchitecture { A.instructionLength = 3
                                , A.wordLength = 1
                                , A.locateArg = locateArg
                                , A.locateStatic = M.fromList [ ("End", -0x1)
                                                              , ("Inc", 0x4)
                                                              , ("Dec", 0x5)
                                                              , ("Z",   0x6)
                                                              , ("T0",  0x8)
                                                              , ("T1",  0x9)
                                                              , ("T2",  0xa)
                                                              , ("T3",  0xb)
                                                              , ("T4",  0xc)
                                                              , ("CW",  0xf)
                                                              -- , ("Lo", 0x120)
                                                              ]
                                , A.writeWord = Mem.write `on` fromIntegral
                                }

subleqMAInitialMem :: M.Map SubleqWord SubleqWord
subleqMAInitialMem = Mem.write 0xf wordLength . Mem.write inc (-1) . Mem.write dec 1 $ M.empty

subleqMATextSection :: Integer
subleqMATextSection = 0x1000

executeSubroutineWithStates :: A.Module -> A.Id -> [SubleqWord] -> Maybe Integer -> Maybe (Maybe ([SubleqWord], Fix2SubleqState), [Fix2SubleqState])
executeSubroutineWithStates mo x args mn = first (fmap (extractArgs &&& id)) . exec mn . fromIntegral <$> M.lookup x pos
    where
      args' = zip [0..] args
      writeArgs = foldr (uncurry Mem.write)
      (_, pos, mem) = A.loadModulePacked subleqMA subleqMATextSection mo subleqMAInitialMem
      extractArgs (_,mem') = map (flip Mem.read mem' . fst) args'
      exec :: Maybe Integer -> SubleqWord -> (Maybe Fix2SubleqState, [Fix2SubleqState])
      exec (Just n') pc = if length (take (n+1) ss) <= n then (Just s, take n ss) else (Nothing, ss)
        where
          (s, ss) = runMachineWithHistory Subleq.step (pc, writeArgs mem args')
          n = fromIntegral n'
      exec Nothing pc = (Just s, ss)
        where
          (s, ss) = runMachineWithHistory Subleq.step (pc, writeArgs mem args')


maximumTry :: Maybe Integer
maximumTry = Just 1000000

executeSubroutine :: A.Id -> [SubleqWord] -> [SubleqWord] 
executeSubroutine x args = case executeSubroutineWithStates subleqModule x args maximumTry of
                             Just (Just (res, _), _) ->  res
                             Just (Nothing, _) -> error "Not terminated"
                             Nothing -> error "Not found"

prop_IntWordTrip :: Int8 -> Bool
prop_IntWordTrip a = a == fromIntegral w
    where
      w :: Word8
      w = fromIntegral a

prop_WordIntTrip :: Word8 -> Bool
prop_WordIntTrip a = a == fromIntegral w
    where
      w :: Int8
      w = fromIntegral a

prop_Add :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Add a b c = [b + c, b, c] == executeSubroutine "add" [a, b, c]

-- prop_Mult :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
-- prop_Mult a b c = [b * c, b, c] == executeSubroutine "mult" [a, b, c]

prop_Multu :: SubleqUWord -> SubleqUWord -> Bool
prop_Multu b c  = fromIntegral a' == b * c
    where
      [a', _, _] = executeSubroutine "multuLo" $ map fromIntegral [0, b, c]

prop_Floor2pow :: NonNegative SubleqWord -> NonNegative SubleqWord -> NonNegative SubleqWord -> Bool
prop_Floor2pow (NonNegative a) (NonNegative b) (NonNegative c)  = a' == a && r1 <= a && (a == 0 || a < 2 * r1) && r2 == a `div` 2
    where
      [r1, r2, a'] = executeSubroutine "floor2pow" [b, c, a]

prop_Bne :: SubleqWord -> SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Bne rs rt off pc  = [rs', rt', off'] == [rs, rt, off] && ((rs == rt && pc' == pc + off) || (rs /= rt && pc' == pc))
    where
      [rs', rt', off', pc'] = executeSubroutine "bne" [rs, rt, off, pc]

prop_BneA :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_BneA rs off pc  = [rs', off'] == [rs, off] && pc' == pc + off
    where
      [rs', off', pc'] = executeSubroutine "bnea" [rs, off, pc]

prop_Sll :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Sll rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` fromIntegral s
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "sll" $ map fromIntegral [rd, rt, s]

prop_Srl :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srl rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (-(fromIntegral s))
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "srl" $ map fromIntegral [rd, rt, s]

prop_Srl1dTest :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srl1dTest rd rh rl  = rd' == s && rl' == rl `shift` 1 && rh' == (rh `shift` 1) + s
    where
      s = rl `shift` (1 - wordLength)
      [rd', rh', rl'] = map fromIntegral $ executeSubroutine "srl1dTest" $ map fromIntegral [rd, rh, rl]

srl1dTestCd :: SubleqUWord -> SubleqUWord -> SubleqUWord -> (SubleqUWord, SubleqUWord, SubleqUWord)
srl1dTestCd rd rh rl  = (s, (rh `shift` 1) + s, rl `shift` 1)
    where
      s = rl `shift` (1 - wordLength)

showFix2SubleqState :: Fix2SubleqState -> Doc
showFix2SubleqState (pc, mem) = integer (fromIntegral pc) <> colon PP.<+> hsep (map (\a-> integer $ memread a mem) [0..15]) PP.<+> colon PP.<+> hsep (map (\a-> integer $ memread a mem) [16..31]) PP.<+> colon PP.<+> integer (memread 0x120 mem) PP.<+> colon PP.<+>  hsep (map (\a-> integer $ memread a mem) [pc..(pc+2)])
    where
      memread a mem = fromIntegral $ Mem.read (fromIntegral a) mem

printExecution :: Maybe (Maybe ([SubleqWord], Fix2SubleqState), [Fix2SubleqState]) -> Doc
printExecution Nothing = text "Subroutine not found"
printExecution (Just (Nothing, ss)) = text "Non terminated" $$ vcat (take 50 $ map showFix2SubleqState ss)
printExecution (Just (Just (args, _), ss)) = text "Terminated" $$ vcat (map showFix2SubleqState ss) $$ text "result: " <> text (show args) <> semi PP.<+> hsep (map (text . printf "%x") args)

return []

printModule = putStrLn $ render $ A.printModule subleqModule

printSubroutine s addr = f loc
    where
      obj = A.lookupModule s subleqModule
      loc = obj >>= A.locate subleqMA addr
      f (Just (o, _)) = putStrLn $ render $ A.printObject o
      f _ = putStrLn "not found"

main :: IO Bool
main = $quickCheckAll
