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

testSubleq :: [IntSubleqState]
testSubleq = snd $ runMachineWithHistory Subleq.step Subleq.initialMachine

locateArg :: A.LocateArg
locateArg xs = M.fromList $ zip xs [0..3]

subleqRoutines :: B.ByteString
subleqRoutines = $(embedFile "subleq-int.sq")

subleqModule :: A.Module
subleqModule = either (error . show) A.expandMacroAll $ parse A.parseModule "parserModule" subleqRoutines

inc, dec :: Integer
inc = 0x4
dec = 0x5

subleqMA :: A.MemoryArchitecture (M.Map Integer Integer)
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
                                                              -- , ("Lo", 0x120)
                                                              ]
                                , A.writeWord = Mem.write
                                }

subleqMAInitialMem :: M.Map Integer Integer
subleqMAInitialMem = Mem.write inc (-1) . Mem.write dec 1 $ M.empty

subleqMATextSection :: Integer
subleqMATextSection = 0x1000

executeSubroutineWithStates :: A.Module -> A.Id -> [Integer] -> Maybe Integer -> Maybe (Maybe ([Integer], IntSubleqState), [IntSubleqState])
executeSubroutineWithStates mo x args mn = first (fmap (extractArgs &&& id)) . exec mn <$> M.lookup x pos
    where
      args' = zip [1..] args
      writeArgs = foldr (uncurry Mem.write)
      (_, pos, mem) = A.loadModulePacked subleqMA subleqMATextSection mo subleqMAInitialMem
      extractArgs (_,mem') = map (flip Mem.read mem' . fst) $ zip [1..] args
      exec :: Maybe Integer -> Integer -> (Maybe IntSubleqState, [IntSubleqState])
      exec (Just n') pc = if length (take (n+1) ss) <= n then (Just s, take n ss) else (Nothing, ss)
        where
          (s, ss) = runMachineWithHistory Subleq.step (pc, writeArgs mem args')
          n = fromIntegral n'
      exec Nothing pc = (Just s, ss)
        where
          (s, ss) = runMachineWithHistory Subleq.step (pc, writeArgs mem args')


maximumTry :: Maybe Integer
maximumTry = Just 1000000

executeSubroutine :: A.Id -> [Integer] -> [Integer] 
executeSubroutine x args = case executeSubroutineWithStates subleqModule x args maximumTry of
                             Just (Just (res, _), _) ->  res
                             Just (Nothing, _) -> error "Not terminated"
                             Nothing -> error "Not found"

prop_Add :: Integer -> Integer -> Integer -> Bool
prop_Add a b c = [b + c, b, c] == executeSubroutine "add" [a, b, c]

prop_Mult :: Integer -> Integer -> Integer -> Bool
prop_Mult a b c = [b * c, b, c] == executeSubroutine "mult" [a, b, c]

prop_Multu :: NonNegative Integer -> NonNegative Integer -> Bool
prop_Multu (NonNegative b) (NonNegative c)  = a' == b * c
    where
      [a', _, _] = executeSubroutine "multu" [0, b, c]

prop_Floor2pow :: NonNegative Integer -> NonNegative Integer -> NonNegative Integer -> Bool
prop_Floor2pow (NonNegative a) (NonNegative b) (NonNegative c)  = a' == a && r1 <= a && (a == 0 || a < 2 * r1) && r2 == a `div` 2
    where
      [r1, r2, a'] = executeSubroutine "floor2pow" [b, c, a]

prop_Bne :: Integer -> Integer -> Integer -> Integer -> Bool
prop_Bne rs rt off pc  = [rs', rt', off'] == [rs, rt, off] && ((rs == rt && pc' == pc + off) || (rs /= rt && pc' == pc))
    where
      [rs', rt', off', pc'] = executeSubroutine "bne" [rs, rt, off, pc]

prop_BneA :: Integer -> Integer -> Integer -> Bool
prop_BneA rs off pc  = [rs', off'] == [rs, off] && pc' == pc + off
    where
      [rs', off', pc'] = executeSubroutine "bnea" [rs, off, pc]

showIntSubleqState :: IntSubleqState -> Doc
showIntSubleqState (pc, mem) = integer pc <> colon PP.<+> hsep (map (\a-> integer $ Mem.read a mem) [0..15]) PP.<+> colon PP.<+> hsep (map (\a-> integer $ Mem.read a mem) [16..31]) PP.<+> colon PP.<+> integer (Mem.read 0x120 mem) PP.<+> colon PP.<+>  hsep (map (\a-> integer $ Mem.read a mem) [pc..(pc+2)])

printExecution :: Maybe (Maybe ([Integer], IntSubleqState), [IntSubleqState]) -> Doc
printExecution Nothing = text "Subroutine not found"
printExecution (Just (Nothing, ss)) = text "Non terminated" $$ vcat (take 50 $ map showIntSubleqState ss)
printExecution (Just (Just (args, _), ss)) = text "Terminated" $$ vcat (map showIntSubleqState ss) $$ text "result: " <> text (show args)

return []
main :: IO Bool
main = $quickCheckAll
