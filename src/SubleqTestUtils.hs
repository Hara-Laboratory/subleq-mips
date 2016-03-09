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

import Data.String
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

measureExecutedInsns :: (Memory w w m, Num w, Enum w, Show w) =>  SubleqProgram w m -> A.Id -> [w] -> Maybe Integer -> Integer
measureExecutedInsns prog x args mn = g $ executeSubroutineWithStates prog x args mn
  where
    g :: SubleqResult a w m -> Integer
    g (Just (Just _, ss)) = fromIntegral $ length ss - 1
    g (Just (Nothing, ss)) = fromIntegral $ length ss - 1
    g Nothing = error $ unlines [ "Invalid result. Maybe non-terminate."
                                , "Insn Limitation: " ++ show mn
                                , x ++ "(" ++ unwords (map show args) ++ ")"
                                ]

measureInsns :: SubleqResult a w m -> Integer
measureInsns (Just (Just _, ss)) = fromIntegral $ length ss - 1
measureInsns (Just (Nothing, ss)) = fromIntegral $ length ss - 1
measureInsns Nothing = error "Invalid result. Maybe non-terminate."


uniformTo :: (R.Distribution R.Uniform a, Num a, Integral b) => b -> R.RVar a
uniformTo n = R.uniform 0 (fromIntegral n)

res :: (Integral a)=>a -> Int -> IO [Double]
-- res :: (Distribution Uniform b, MonadRandom f, Functor f, Floating b) => Int -> f [b]
res m n = map (2 **) <$> ( replicateM n . R.sample $ uniformTo m)

readTraceFromFile :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m, Show w)=>String -> SubleqProgram w m -> FilePath -> IO ()
readTraceFromFile prefix prog filename = do
    let outputfilename = FP.replaceBaseName filename (prefix ++ FP.takeBaseName filename)
    f <- BL.readFile filename
    either (\x -> return ()) id $ BL.writeFile outputfilename . CSV.encode . M.toList <$> readTrace prog f

readTraceFromFiles :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m, Show w)=>FilePath -> SubleqProgram w m -> [FilePath] -> IO ()
readTraceFromFiles output prog filenames = do
    fs <- mapM BL.readFile filenames
    let anss = either error id $ mapM (readTrace prog) fs :: [Map String Integer]
    let fields = M.unionsWith M.union . map (\(name,insns) -> M.map (M.singleton name) insns) $ zip filenames anss :: Map String (Map String Integer)
    let fields' = M.mapWithKey (\k v -> M.insert "insn" k $ M.map show v) fields
    BL.writeFile output . CSV.encodeByName (V.fromList  $ map fromString ("insn" : filenames)) . M.elems $ fields'

-- data SubleqArguments w = LoadStore w w w
--    deriving (Show, Read, Eq, Ord)
type SubleqArguments w = (w, w, w, w)

bitReversal :: (FiniteBits a) => a -> a
bitReversal n = foldl setBit zeroBits bs'
    where
      l = finiteBitSize n
      bs = filter (testBit n) [0..(l-1)]
      bs' = map (\x -> l - x - 1) bs

parseTrace :: (Num w, Enum w, Read w)=>BL.ByteString -> Either String (Vector (String, SubleqArguments w))
parseTrace = fmap (V.map conv) . CSV.decode CSV.NoHeader
    where
      conv (insn, offset, src1, src2, res_lw) = (insn, (Prelude.read offset, Prelude.read src1, Prelude.read src2, Prelude.read res_lw))


traceExecute :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m, Show w)=>SubleqProgram w m -> [(String, SubleqArguments w)] -> [(String, Integer)]
traceExecute prog = map (\(insn, args) -> (insn, f prog insn args))


idealSituation = False
idealSituationW = False
idealSituationHB = False

f :: (Integral w, Bits w, Memory w w m, Show w)=>SubleqProgram w m -> String -> SubleqArguments w -> Integer
f prog insn args | insn `elem` ["sb", "sh"] = load prog (ncycle insn) args + nSwSub1
                 | insn `elem` ["lbu", "lb" , "lhu", "lh"] = load prog (ncycle insn) args
f prog insn (_, rt, sa, ans) | insn `elem` ["sll", "srl", "sra"] = measureShift prog insn rt sa ans
f prog "mfxx" _ = 4
f prog "addu" _ = 5
f prog "subu" _ = 7 -- FIX
f prog insn (_, a, b, ans) | insn `elem` ["and", "or", "xor", "nor"] = measureArith prog insn a b ans
f prog insn (_, a, b, ans) | insn `elem` ["mult"] = measureArith prog "multu" a b ans
f prog "lw" (off, addr, rd, mval) = (if idealSituationW then measureShift prog "srl" (off + addr) 2 ((off + addr) `shift` (-2)) + 4 else measureAddr prog "addrw" (off + addr) + 4) + nLwSub1
f prog "sw" (off, addr, rd, mval) = (if idealSituationW then measureShift prog "srl" (off + addr) 2 ((off + addr) `shift` (-2)) + 4 else measureAddr prog "addrw" (off + addr) + 4) + nSwSub1
-- f prog "lw" _  = nLwSub1 -- In case of IDEAL situation: without addres conversion.
-- f prog "sw" _  = nSwSub1 -- In case of IDEAL situation

f prog "lui" (_, r,_, _)  = measureMonadic prog "lui" r
f prog "j" _    = 1
f prog "jal" _  = 3
f prog "jr" _   = 4

f prog "slt"   (_, rs, rt, ans)  = measureArith prog "slt" rs rt ans
f prog "sltu"  (_, rs, rt, ans)  = measureArith prog "sltu" rs rt ans
f prog "slti"  (_, rs, rt, ans)  = measureArith prog "slt" rs rt ans
f prog "sltiu" (_, rs, rt, ans)  = measureArith prog "sltu" rs rt ans

f prog "nop" _    = 1

f prog insn (_, rs, rt, _) | insn `elem` ["bne", "beq"] = 2 + nJnzp (rs - rt) -- In case of IDEAL situation: Reordered instructions

f prog insn (_, r, _, _) | insn `elem` ["bltz", "bgez", "beqz", "blez", "bgtz"] = nJnzp r

f prog insn args    = error $ "unrecognized instruction: " ++ insn ++ " " ++ show args

allTypes = [ "sb", "sh"
           , "lbu", "lb" , "lhu", "lh"
           , "sll", "srl", "sra"
           , "mfxx"
           , "addu"
           , "subu"
           , "and", "or", "xor", "nor"
           , "mult"
           , "lw" , "sw"
           , "lui" , "j" , "jal" , "jr"
           , "slt" , "sltu" , "slti" , "sltiu"
           , "bne", "beq"
           , "bltz", "bgez", "beqz", "blez", "bgtz"
           ]


-- traceExecute :: (Num w, Enum w, Read w, Memory w w m)=>[(String, SubleqArguments w)] -> [(String, Integer)]
load prog (addrRoutine, liRoutine, posF) (off, addr, rd, mval) = addrConv + nLwSub1 + valConv
    where
      addr' = off + addr
      --addrConv = measureAddr prog addrRoutine addr'
      addrConv = if idealSituationHB then 0 else measureAddr prog addrRoutine addr'
      valConv = measureLoad prog liRoutine (posF addr') mval rd

measureMonadic prog sub a = measureExecutedInsns prog sub [0,a] Nothing

measureArith prog sub a b ans = measureExecutedInsns prog sub [0,a,b] Nothing
measureShift prog sub val sa ans = measureExecutedInsns prog sub [0,val,sa] Nothing

measureAddr prog sub addr = measureExecutedInsns prog (sub ++ "Test") [0,0,addr] Nothing
measureLoad prog sub pos mval rd = measureExecutedInsns prog (sub ++ "Test") [rd,mval,pos] Nothing

nJnzp n = g (n `compare` 0)
    where
      g LT = 3
      g EQ = 3
      g GT = 2

nLwSub1 = 6
nSwSub1 = 10
ncycle :: (Num a, Bits a, Integral a) => String -> (String, String, a -> a)
ncycle "lbu" = ("addrb", "lbui", (`mod` 4))
ncycle "lb"  = ("addrb", "lbui", (`mod` 4))
ncycle "sb"  = ("addrb", "sbi",  (`mod` 4))
ncycle "lhu" = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "lh"  = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "sh"  = ("addrh", "shi",  \ n -> (n `mod` 4) `shift` (-1))

readTrace :: (Num w, Enum w, Read w, Integral w, Bits w, Memory w w m, Show w)=>SubleqProgram w m -> BL.ByteString -> Either String (Map String Integer)
readTrace prog trace = M.unionWith (+) (M.fromList $ map (\x -> (x, 0)) allTypes) . M.fromListWith (+) . traceExecute prog . V.toList <$> parseTrace trace

