{-# LANGUAGE FlexibleContexts #-}
module SubleqTestUtils where

import Language.Subleq.Model.Prim
import Language.Subleq.Model.Memory as Mem
import Language.Subleq.Model.Architecture.IntMachine
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
import Control.Monad
import Control.Arrow
import Test.QuickCheck
-- import Test.QuickCheck.Text
-- import Test.QuickCheck.All
import Data.Word
import Data.Int
import Data.Bits
import Data.Function
import Text.Printf

import qualified Data.ByteString.Lazy as BL
import qualified Data.Random as R
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Uniform as R

import qualified Data.Csv as CSV
import Data.Vector(Vector)
import qualified Data.Vector as V
import qualified System.FilePath as FP

subleqMA = undefined
subleqMATextSection = undefined
subleqMAInitialMem = undefined

type SubleqProgram w m = (Machine w w m Bool, Map A.Id Integer, m)

-- assembleMachine :: A.Module -> (Map Id Integer, m)
assembleMachine :: A.MemoryArchitecture m -> Integer -> A.Module -> m -> (Map A.Id Integer, m)
assembleMachine ma text mo initmem = (pos, mem)
    where
      (_, pos, mem) = A.loadModulePacked ma text mo initmem

executeSubroutineWithStates :: (Memory w w m, Num w, Enum w) =>  SubleqProgram w m -> A.Id -> [w] -> Maybe Integer -> Maybe (Maybe ([w], SubleqState w w m), [SubleqState w w m])
executeSubroutineWithStates (step, pos, mem) x args mn = o . e <$> M.lookup x pos
    where
      -- e :: (Memory a w m, Num w) => Integer -> (Maybe (SubleqState a w m), [SubleqState a w m])
      e = exec step mem args mn . fromIntegral
      o = first (fmap (extractArgs args &&& id))

changedAddresses = undefined

-- executeSubroutineWithModification :: A.Id -> [SubleqWord] -> ([SubleqWord], Set SubleqWord)
executeSubroutineWithModification :: (Memory w w m, Num w, Enum w) => SubleqProgram w m -> A.Id -> [w] -> Maybe Integer -> ([w], Set w)
executeSubroutineWithModification (step, pos, mem) x args maximumTry = case executeSubroutineWithStates (step, pos, mem) x args maximumTry of
                             Just (Just (res, end), init:_) ->  (res, changedAddresses end init)
                             Just (Nothing, _) -> error "Not terminated"
                             Nothing -> error "Not found"

indexArgs :: (Enum a, Num a) => [b] -> [(a, b)]
indexArgs = zip [0..]

extractArgs :: (Memory a w m, Enum a, Num a, Num w) => [w] -> (c, m) -> [w]
extractArgs args (_,mem') = map (flip Mem.read mem' . fst) $ indexArgs args

writeArgs :: (Memory a w m, Num w, Num a, Enum a) => m -> [w] -> m
writeArgs mem = foldr (uncurry Mem.write) mem . indexArgs

exec :: (Num w, Memory w w m, Enum w)=>Machine w w m Bool -> m -> [w] -> Maybe Integer -> w -> (Maybe (SubleqState w w m), [SubleqState w w m])
exec step mem args (Just n') pc = if length (take (n+1) ss) <= n then (Just s, take n ss) else (Nothing, ss)
  where
    (s, ss) = runMachineWithHistory step (pc, writeArgs mem args)
    n = fromIntegral n'
exec step mem args Nothing pc = (Just s, ss)
  where
    (s, ss) = runMachineWithHistory step (pc, writeArgs mem args)

type SubleqResult a w m = Maybe (Maybe ([w], SubleqState a w m), [SubleqState a w m])

measureInsns :: SubleqResult a w m -> Integer
measureInsns (Just (Just _, ss)) = fromIntegral $ length ss - 1
measureInsns (Just (Nothing, ss)) = fromIntegral $ length ss - 1
measureInsns Nothing = error "Invalid result. Maybe non-terminate."


uniformTo :: (R.Distribution R.Uniform a, Num a, Integral b) => b -> R.RVar a
uniformTo n = R.uniform 0 (fromIntegral n)

res :: (Integral a)=>a -> Int -> IO [Double]
-- res :: (Distribution Uniform b, MonadRandom f, Functor f, Floating b) => Int -> f [b]
res m n = map (2 **) <$> ( replicateM n . R.sample $ uniformTo m)

readTraceFromFile :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m)=>SubleqProgram w m -> FilePath -> IO ()
readTraceFromFile prog filename = do
    let outputfilename = FP.replaceBaseName filename ("measure-subleq-" ++ FP.takeBaseName filename)
    f <- BL.readFile filename
    either (\x -> return ()) id $ BL.writeFile outputfilename . CSV.encode . M.toList <$> readTrace prog f

data SubleqArguments w = LoadStore w w w
    deriving (Show, Read, Eq, Ord)

parseTrace :: (Num w, Enum w, Read w)=>BL.ByteString -> Either String (Vector (String, SubleqArguments w))
parseTrace = fmap (V.map conv) . CSV.decode CSV.NoHeader
    where
      conv (insn, offset, src1, src2, res_lw) = (insn, LoadStore (Prelude.read offset + Prelude.read src1) (Prelude.read src2) (Prelude.read res_lw))


traceExecute :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m)=>SubleqProgram w m -> [(String, SubleqArguments w)] -> [(String, Integer)]
traceExecute prog = map (\(insn, args) -> (insn, f prog insn args))

f :: (Integral w, Bits w, Memory w w m)=>SubleqProgram w m -> String -> SubleqArguments w -> Integer
f prog insn args | insn `elem` ["lbu", "lb" , "sb" , "lhu", "lh" , "sh"] = load prog (ncycle insn) args
f prog "lw" args = 6

-- traceExecute :: (Num w, Enum w, Read w, Memory w w m)=>[(String, SubleqArguments w)] -> [(String, Integer)]
load prog (addrRoutine, liRoutine, posF) (LoadStore addr rd mval) = 12 + measureAddr prog addrRoutine addr + nLwSub1 + measureLoad prog liRoutine (posF addr) mval rd

measureAddr prog sub addr = measureInsns $ executeSubroutineWithStates prog (sub ++ "Test") [0,0,addr] Nothing
measureLoad prog sub pos mval rd = measureInsns $ executeSubroutineWithStates prog (sub ++ "Test") [rd,mval,pos] Nothing
nLwSub1 = 6
nSwSub1 = 10
ncycle :: (Num a, Bits a, Integral a) => String -> (String, String, a -> a)
ncycle "lbu" = ("addrb", "lbui", (`mod` 4))
ncycle "lb"  = ("addrb", "lbui", (`mod` 4))
ncycle "sb"  = ("addrb", "sbi",  (`mod` 4))
ncycle "lhu" = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "lh"  = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "sh"  = ("addrh", "shi",  \ n -> (n `mod` 4) `shift` (-1))

readTrace :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m)=>SubleqProgram w m -> BL.ByteString -> Either String (Map String Integer)
readTrace prog trace = M.fromListWith (+) . traceExecute prog . V.toList <$> parseTrace trace

