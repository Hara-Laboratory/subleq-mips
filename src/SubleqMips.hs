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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
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

{-
type SubleqWord = Int32
type SubleqUWord = Word32
wordLength :: (Integral a, Num a) => a
wordLength = 32
-}

type SubleqWord = Int16
type SubleqUWord = Word16
wordLength :: (Integral a, Num a) => a
wordLength = 16


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
                                                              , ("T5",  0xd)
                                                              , ("T6",  0xe)
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

executeSubroutineWithModification :: A.Id -> [SubleqWord] -> ([SubleqWord], Set SubleqWord)
executeSubroutineWithModification x args = case executeSubroutineWithStates subleqModule x args maximumTry of
                             Just (Just (res, end), init:_) ->  (res, changedAddresses end init)
                             Just (Nothing, _) -> error "Not terminated"
                             Nothing -> error "Not found"

executeSubroutine :: A.Id -> [SubleqWord] -> [SubleqWord] 
executeSubroutine x args = if diffs `S.isSubsetOf` S.fromList [0..(fromIntegral $ length args - 1)] then res else error $ "it corrupses: " ++ show diffs
    where
      (res, diffs) = executeSubroutineWithModification x args


multD hi lo rs rt  = ([(iHi `shift` wordLength) + iLo, iHi `shift` wordLength, iHi, iLo, iRs, iRt], (iHi `shift` wordLength) + iLo == iRs * iRt)
    where
      iHi, iLo, iRs, iRt :: Integer
      iLo = fromIntegral (fromIntegral lo' :: SubleqUWord)
      [iHi, iRs, iRt] = map fromIntegral [hi', rs, rt]
      [hi', lo', _, _] = executeSubroutine "multD" [hi, lo, rs, rt]

showFix2SubleqState :: Fix2SubleqState -> Doc
showFix2SubleqState (pc, mem) = integer (fromIntegral pc) <> colon PP.<+> hsep (map (\a-> integer $ memread a mem) [0..15]) PP.<+> colon PP.<+> hsep (map (\a-> integer $ memread a mem) [16..31]) PP.<+> colon PP.<+> integer (memread 0x120 mem) PP.<+> colon PP.<+>  hsep (map (\a-> integer $ memread a mem) [pc..(pc+2)])
    where
      memread a mem = fromIntegral $ Mem.read (fromIntegral a) mem

printExecution :: Maybe (Maybe ([SubleqWord], Fix2SubleqState), [Fix2SubleqState]) -> Doc
printExecution Nothing = text "Subroutine not found"
printExecution (Just (Nothing, ss)) = text "Non terminated" $$ vcat (take 50 $ map showFix2SubleqState ss)
printExecution (Just (Just (args, end), ss)) = status $$ history $$ result $$ memoryDiff
    where
      status = text "Terminated"
      history = vcat (map showFix2SubleqState ss)
      result = text "result: " <> text (show args) <> semi PP.<+> hsep (map (text . printf "%x") args)
      memoryDiff = text "modified: " <> text (show $ changedAddresses end $ head ss)

changedAddresses :: Fix2SubleqState -> Fix2SubleqState -> Set SubleqWord
changedAddresses (_, mem) (_,init) = M.foldrWithKey (\k v a-> if v then S.insert k a else a) S.empty  $ M.mergeWithKey f g g mem init -- M.mergeWithKey (/=) mem init
    where
      f :: SubleqWord -> SubleqWord -> SubleqWord -> Maybe Bool
      f _ m1 m2 | m1 == m2 = Nothing
                | otherwise = Just True
      g ::  Map SubleqWord SubleqWord -> Map SubleqWord Bool
      g = M.map (const True) . M.filter (/= 0)

printModule = putStrLn $ render $ A.printModule subleqModule

printSubroutine s addr = f loc
    where
      obj = A.lookupModule s subleqModule
      loc = obj >>= A.locate subleqMA addr
      f (Just (o, _)) = putStrLn $ render $ A.printObject o
      f _ = putStrLn "not found"

return []

main :: IO Bool
main = $quickCheckAll
