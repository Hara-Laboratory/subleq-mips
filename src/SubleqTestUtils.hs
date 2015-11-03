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

import qualified Data.Random as R
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Uniform as R

subleqMA = undefined
subleqMATextSection = undefined
subleqMAInitialMem = undefined

-- assembleMachine :: A.Module -> (Map Id Integer, m)
assembleMachine :: A.MemoryArchitecture m -> Integer -> A.Module -> m -> (Map A.Id Integer, m)
assembleMachine ma text mo initmem = (pos, mem)
    where
      (_, pos, mem) = A.loadModulePacked ma text mo initmem

executeSubroutineWithStates :: (Memory w w m, Num w, Enum w) => Machine w w m Bool -> Map A.Id Integer -> m -> A.Id -> [w] -> Maybe Integer -> Maybe (Maybe ([w], SubleqState w w m), [SubleqState w w m])
executeSubroutineWithStates step pos mem x args mn = o . e <$> M.lookup x pos
    where
      -- e :: (Memory a w m, Num w) => Integer -> (Maybe (SubleqState a w m), [SubleqState a w m])
      e = exec step mem args mn . fromIntegral
      o = first (fmap (extractArgs args &&& id))

changedAddresses = undefined

-- executeSubroutineWithModification :: A.Id -> [SubleqWord] -> ([SubleqWord], Set SubleqWord)
executeSubroutineWithModification :: (Memory w w m, Num w, Enum w) => Machine w w m Bool -> Map A.Id Integer -> m -> A.Id -> [w] -> Maybe Integer -> ([w], Set w)
executeSubroutineWithModification step pos mem x args maximumTry = case executeSubroutineWithStates step pos mem x args maximumTry of
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


uniformTo :: (R.Distribution R.Uniform a, Num a, Integral b) => b -> R.RVar a
uniformTo n = R.uniform 0 (fromIntegral n)

res :: (Integral a)=>a -> Int -> IO [Double]
-- res :: (Distribution Uniform b, MonadRandom f, Functor f, Floating b) => Int -> f [b]
res m n = map (2 **) <$> ( replicateM n . R.sample $ uniformTo m)
