{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, OverloadedStrings #-}
module Main where

import Language.Subleq.Model.Prim hiding (read)
import Language.Subleq.Model.Memory hiding (read)
import Language.Subleq.Model.Memory as Mem
import Language.Subleq.Model.Architecture.IntMachine hiding (read)
-- import qualified Language.Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Language.Subleq.Model.InstructionSet.SubleqR as Subleq
import qualified Language.Subleq.Assembly as A
import qualified Language.Subleq.Assembly.Export.Elf2Mem as A
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint hiding ((<+>))
import qualified Text.PrettyPrint as PP
import qualified Data.ByteString as B
-- import Data.Maybe
import Data.String.ToString
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.FileEmbed
import Control.Monad
-- import Control.Monad.State
-- import Control.Lens
import Control.Arrow
import Test.QuickCheck
-- import Test.QuickCheck.Text
-- import Test.QuickCheck.All
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Bits as Bit
import Data.Function
import Text.Printf
import System.Environment

import qualified Data.ByteString.Lazy as BL
import qualified Data.Random as R
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Uniform as R

import qualified Data.Csv as CSV
import Data.Vector(Vector)
import qualified Data.Vector as V
import qualified System.FilePath as FP

import qualified SubleqTestUtils as T

type SubleqWord = Int32
type SubleqUWord = Word32
wordLength :: (Integral a, Num a) => a
wordLength = 32

{-
type SubleqWord = Int16
type SubleqUWord = Word16
wordLength :: (Integral a, Num a) => a
wordLength = 16
-}


type Fix2SubleqMemory = M.Map SubleqWord SubleqWord
type Fix2SubleqState = SubleqState SubleqWord SubleqWord Fix2SubleqMemory

locateArg :: A.LocateArg
locateArg xs = M.fromList $ zip xs [0..3]

subleqRoutines :: B.ByteString
subleqRoutines = $(embedFile "subleqr-int-fix2.sq")

subleqModule :: A.Module
subleqModule = either (error . show) A.expandMacroAll $ parse A.parseModule "parserModule" subleqRoutines

inc, dec :: SubleqWord
inc = 0x4
dec = 0x5

subleqMA :: A.MemoryArchitecture (M.Map SubleqWord SubleqWord)
subleqMA = A.MemoryArchitecture { A.instructionLength = 3
                                , A.wordLength = 1
                                , A.locateArg = locateArg
                                , A.locateStatic = M.fromList [ ("End", 0)
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
                                                              -- , ("Min",  0x0d)
                                                              -- , ("Max",  0x0e)
                                                              , ("CW",  0xf)
                                                              , ("Min",  0x10)
                                                              , ("Max",  0x11)
                                                              , ("WordLength",  32)
                                                              ]
                                , A.writeWord = Mem.write `on` fromIntegral
                                }

subleqMAInitialMem :: M.Map SubleqWord SubleqWord
subleqMAInitialMem =  Mem.write 0x11 maxBound . Mem.write 0x10 minBound . Mem.write 0xf wordLength . Mem.write inc (-1) . Mem.write dec 1 $ M.empty
-- subleqMAInitialMem =  Mem.write 0x0e maxBound . Mem.write 0x0d minBound . Mem.write 0xf wordLength . Mem.write inc (-1) . Mem.write dec 1 $ M.empty
{-
subleqMAInitialMem =  foldr (<$) M.empty [ Mem.write 0xf wordLength
                                         , Mem.write inc (-1)
                                         , Mem.write dec 1
                                         ]
-}
subleqMATextSection :: Integer
subleqMATextSection = 0x100

executeSubroutineWithStates :: A.Id -> [SubleqWord] -> Maybe Integer -> Maybe (Maybe ([SubleqWord], Fix2SubleqState), [Fix2SubleqState])
executeSubroutineWithStates x args mn = T.executeSubroutineWithStates Subleq.step pos mem x args mn
    where
      (pos, mem) = T.assembleMachine subleqMA subleqMATextSection subleqModule subleqMAInitialMem

maximumTry :: Maybe Integer
-- maximumTry = Just 1000000
maximumTry = Just 10000

executeSubroutineWithModification :: A.Id -> [SubleqWord] -> ([SubleqWord], Set SubleqWord)
executeSubroutineWithModification x args = case executeSubroutineWithStates x args maximumTry of
                             Just (Just (res, end), init:_) ->  (res, changedAddresses end init)
                             Just (Nothing, _) -> error "Not terminated"
                             Nothing -> error "Not found"

executeSubroutine :: A.Id -> [SubleqWord] -> [SubleqWord] 
executeSubroutine x args = if diffs `S.isSubsetOf` S.fromList [0..(fromIntegral $ length args - 1)] then res else error $ "it corrupses: " ++ show diffs
    where
      (res, diffs) = executeSubroutineWithModification x args

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
prop_Add a b c = [b + c, b, c] == executeSubroutine "addu" [a, b, c]

prop_Sub :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Sub a b c = [b - c, b, c] == executeSubroutine "subu" [a, b, c]

prop_Mflo :: SubleqWord -> SubleqWord -> Bool
prop_Mflo a lo = [lo, lo] == executeSubroutine "mflo" [a, lo]

prop_Mtlo :: SubleqWord -> SubleqWord -> Bool
prop_Mtlo lo a = [a, a] == executeSubroutine "mtlo" [lo, a]

-- prop_Mult :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
-- prop_Mult a b c = [b * c, b, c] == executeSubroutine "mult" [a, b, c]

prop_JezoTest :: SubleqUWord -> Bool
prop_JezoTest a  = fromIntegral a' == f a
    where
      [a'] = executeSubroutine "jezoTest" $ map fromIntegral [a]
      f 0 = 0
      f x | even x = 1
          | odd  x = -1

-- prop_MultuLo :: SubleqUWord -> SubleqUWord -> Bool
-- prop_MultuLo b c  = fromIntegral a' == b * c
--     where
--       [a', _, _] = executeSubroutine "multuLo" $ map fromIntegral [0, b, c]
-- 
-- prop_Floor2pow :: NonNegative SubleqWord -> NonNegative SubleqWord -> NonNegative SubleqWord -> Bool
-- prop_Floor2pow (NonNegative a) (NonNegative b) (NonNegative c)  = a' == a && r1 <= a && (a == 0 || a < 2 * r1) && r2 == a `div` 2
--     where
--       [r1, r2, a'] = executeSubroutine "floor2pow" [b, c, a]

prop_Bne :: SubleqWord -> SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Bne rs rt off pc  = [rs', rt', off'] == [rs, rt, off] && ((rs == rt && pc' == pc + off) || (rs /= rt && pc' == pc))
    where
      [rs', rt', off', pc'] = executeSubroutine "bne" [rs, rt, off, pc]

prop_BneA :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_BneA rs off pc  = [rs', off'] == [rs, off] && pc' == pc + off
    where
      [rs', off', pc'] = executeSubroutine "bnea" [rs, off, pc]

prop_Lui :: SubleqUWord -> SubleqUWord -> Bool
prop_Lui rt imm  = imm' == imm && rt' == imm `shift` 16
    where
      [rt', imm'] = map fromIntegral $ executeSubroutine "lui" $ map fromIntegral [rt, imm]

prop_Sll :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Sll rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` fromIntegral s
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "sll" $ map fromIntegral [rd, rt, s]

prop_Slli3 :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Slli3 rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (fromIntegral s `shift` 3)
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "slli3" $ map fromIntegral [rd, rt, s]

prop_Slli4 :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Slli4 rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (fromIntegral s `shift` 4)
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "slli4" $ map fromIntegral [rd, rt, s]

prop_Srl :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srl rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (-(fromIntegral s))
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "srl" $ map fromIntegral [rd, rt, s]

prop_Srli3 :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srli3 rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (- (fromIntegral s `shift` 3))
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "srli3" $ map fromIntegral [rd, rt, s]

prop_Srli4 :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srli4 rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (- (fromIntegral s `shift` 4))
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "srli4" $ map fromIntegral [rd, rt, s]

prop_Srl1Test :: SubleqUWord -> SubleqUWord -> Bool
prop_Srl1Test rd rs  = rs' == rs && rd' == rs `shift` (-1)
    where
      [rd', rs'] = map fromIntegral $ executeSubroutine "srl1Test" $ map fromIntegral [rd, rs]

prop_Srl1dTest :: SubleqUWord -> SubleqUWord -> Bool
prop_Srl1dTest rh rl  = rl' == rl `shift` 1 && rh' == (rh `shift` 1) + s
    where
      s = rl `shift` (1 - wordLength)
      [rh', rl'] = map fromIntegral $ executeSubroutine "srl1dTest" $ map fromIntegral [rh, rl]

prop_Srl1dcTest :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Srl1dcTest rd rh rl  = rd' == s && rl' == rl `shift` 1 && rh' == (rh `shift` 1) + s
    where
      s = rl `shift` (1 - wordLength)
      [rd', rh', rl'] = map fromIntegral $ executeSubroutine "srl1dcTest" $ map fromIntegral [rd, rh, rl]

srl1dTestCd :: SubleqUWord -> SubleqUWord -> SubleqUWord -> (SubleqUWord, SubleqUWord, SubleqUWord)
srl1dTestCd rd rh rl  = (s, (rh `shift` 1) + s, rl `shift` 1)
    where
      s = rl `shift` (1 - wordLength)

prop_Sra :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Sra rd rt sa  = [rt', s'] == [rt, s] && rd' == rt `shift` (-(fromIntegral s))
    where
      s = sa `mod` wordLength
      [rd', rt', s'] = map fromIntegral $ executeSubroutine "sra" $ map fromIntegral [rd, rt, s]

prop_Multu :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Multu hi lo rs rt  = rs' == rs && rt' == rt && (iHi `shift` wordLength) + iLo == iRs * iRt
    where
      iHi, iLo, iRs, iRt :: Integer
      [iHi, iLo, iRs, iRt] = map fromIntegral [hi', lo', rs', rt']
      [hi', lo', rs', rt'] = map fromIntegral $ executeSubroutine "multu" $ map fromIntegral [hi, lo, rs, rt]

prop_MultD :: SubleqWord -> SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_MultD hi lo rs rt  = (iHi `shift` wordLength) + iLo == iRs * iRt
    where
      iHi, iLo, iRs, iRt :: Integer
      iLo = fromIntegral (fromIntegral lo' :: SubleqUWord)
      [iHi, iRs, iRt] = map fromIntegral [hi', rs, rt]
      [hi', lo', _, _] = executeSubroutine "multD" [hi, lo, rs, rt]

prop_Mult :: SubleqWord -> SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Mult hi lo rs rt  = [rs', rt'] == [rs, rt] && (iHi `shift` wordLength) + iLo == iRs * iRt
    where
      iHi, iLo, iRs, iRt :: Integer
      iLo = fromIntegral (fromIntegral lo' :: SubleqUWord)
      [iHi, iRs, iRt] = map fromIntegral [hi', rs, rt]
      [hi', lo', rs', rt'] = executeSubroutine "mult" [hi, lo, rs, rt]

prop_Slt :: SubleqWord -> SubleqWord -> SubleqWord -> Bool
prop_Slt rd rs rt  = [rd', rs', rt'] == [if rs < rt then 1 else 0, rs, rt]
    where
      [rd', rs', rt'] = executeSubroutine "slt"  [rd, rs, rt]

prop_Sltu :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Sltu rd rs rt  = [rd', rs', rt'] == [if rs < rt then 1 else 0, rs, rt]
    where
      [rd', rs', rt'] = map fromIntegral $ executeSubroutine "sltu" $ map fromIntegral [rd, rs, rt]

prop_And :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_And rd rs rt  = [rd', rs', rt'] == [rs Bit..&. rt, rs, rt]
    where
      [rd', rs', rt'] = map fromIntegral $ executeSubroutine "and" $ map fromIntegral [rd, rs, rt]

prop_Or :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Or rd rs rt  = [rd', rs', rt'] == [rs Bit..|. rt, rs, rt]
    where
      [rd', rs', rt'] = map fromIntegral $ executeSubroutine "or" $ map fromIntegral [rd, rs, rt]

prop_Xor :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Xor rd rs rt  = [rd', rs', rt'] == [rs `xor` rt, rs, rt]
    where
      [rd', rs', rt'] = map fromIntegral $ executeSubroutine "xor" $ map fromIntegral [rd, rs, rt]

prop_Nor :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Nor rd rs rt  = [rd', rs', rt'] == [complement $ rs Bit..|. rt, rs, rt]
    where
      [rd', rs', rt'] = map fromIntegral $ executeSubroutine "nor" $ map fromIntegral [rd, rs, rt]

prop_Not :: SubleqUWord -> SubleqUWord -> Bool
prop_Not rd rs  = [rd', rs'] == [complement rs, rs]
    where
      [rd', rs'] = map fromIntegral $ executeSubroutine "not" $ map fromIntegral [rd, rs]

prop_Rr :: SubleqUWord -> SubleqUWord -> Bool
prop_Rr rt saB = [sa] == [sa] && rt' == (rt `shift` (-s)) Bit..|. (rt `shift` (wordLength - s))
    where
      s = fromIntegral sa
      sa = saB `mod` wordLength
      [rt', sa'] = map fromIntegral $ executeSubroutine "rrTest" $ map fromIntegral [rt, sa]

prop_RrSrm :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_RrSrm rt rs saB = [sa] == [sa] && rt' == (rt `shift` (-s)) Bit..|. (rs `shift` (wordLength - s))
    where
      s = fromIntegral sa
      sa = saB `mod` wordLength
      [rt', rs', sa'] = map fromIntegral $ executeSubroutine "rrsrmTest" $ map fromIntegral [rt, rs, sa]

prop_Rl :: SubleqUWord -> SubleqUWord -> Bool
prop_Rl rt saB = [sa] == [sa] && rt' == (rt `shift` s) Bit..|. (rt `shift` (s - wordLength))
    where
      s = fromIntegral sa
      sa = saB `mod` wordLength
      [rt', sa'] = map fromIntegral $ executeSubroutine "rlTest" $ map fromIntegral [rt, sa]

prop_RlSlm :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_RlSlm rt rs saB = [sa] == [sa] && rt' == (rt `shift` s) Bit..|. (rs `shift` (s - wordLength)) && rs' == (rs `shift` s)
    where
      s = fromIntegral sa
      sa = saB `mod` wordLength
      [rt', rs', sa'] = map fromIntegral $ executeSubroutine "rlslmTest" $ map fromIntegral [rt, rs, sa]

testSb :: Int -> LSvi -> String -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
testSb n lsvi subr rt rs frB = fr' == fr'' && rs' == lsvi rs rt f s
    where
      s = fromIntegral sa
      f = fromIntegral (fr * sa)
      sa = 1 `shift` n
      fr = frB `mod` (wordLength `div` sa)
      fr'' = fr * (sa `div` 8)
      [rt', rs', fr'] = map fromIntegral $ executeSubroutine subr $ map fromIntegral [rt, rs, fr'']

testLb :: Int -> LSvi -> String -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
testLb n lsvi subr rt rs frB = fr' == fr'' && rt' == lsvi rt rs f s
    where
      s = fromIntegral sa
      f = fromIntegral (fr * sa)
      sa = 1 `shift` n
      fr = frB `mod` (wordLength `div` sa)
      fr'' = fr * (sa `div` 8)
      [rt', rs', fr'] = map fromIntegral $ executeSubroutine subr $ map fromIntegral [rt, rs, fr'']

prop_Sw2  = testSb 5 (\rt rs f s -> rs)  "swTest"
prop_Lw2  = testLb 5 (\rt rs f s -> rs) "lwTest"
prop_Sh   = testSb 4 svi  "shTest"
prop_Lhu  = testLb 4 lvui "lhuTest"
prop_Sb   = testSb 3 svi  "sbTest"
prop_Lbu  = testLb 3 lvui "lbuTest"

mask f s = ((1 `shift` s) - 1) `shift` (f - s + 1)

type LSvi = (SubleqUWord -> SubleqUWord -> Int -> Int -> SubleqUWord)
-- svi rt rs f s = (rt Bit..&. complement (mask f s)) Bit..|. ((rs Bit..&. mask (wordLength - 1) s) `shift` (f - wordLength + 1))
svi :: LSvi
svi rt rs f s = (rt Bit..&. complement (mask (f+s-1) s)) Bit..|. ((rs Bit..&. mask (s - 1) s) `shift` f)

testLSvi :: LSvi -> String -> SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
testLSvi lsvi subr rt rs frB saB = [fr', sa'] == [fr, sa] && rt' == lsvi rt rs f s
    where
      s = fromIntegral sa
      f = fromIntegral fr
      sa = saB `mod` (wordLength - fr + 1)
      fr = frB `mod` wordLength
      [rt', rs', fr', sa'] = map fromIntegral $ executeSubroutine subr $ map fromIntegral [rt, rs, fr, sa]

testLSbi :: Int -> LSvi -> String -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
testLSbi n lsvi subr rt rs frB = fr' == fr && rt' == lsvi rt rs f s
    where
      s = fromIntegral sa
      f = fromIntegral (fr * sa)
      sa = 1 `shift` n
      fr = frB `mod` (wordLength `div` sa)
      [rt', rs', fr'] = map fromIntegral $ executeSubroutine subr $ map fromIntegral [rt, rs, fr]

prop_Svi :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Svi = testLSvi svi "sviTest"

prop_Svli :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Svli = testLSvi svi "svliTest"

prop_Svri :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Svri = testLSvi svi "svriTest"

lvui :: LSvi
-- svi rt rs f s = (rt Bit..&. complement (mask f s)) Bit..|. ((rs Bit..&. mask (wordLength - 1) s) `shift` (f - wordLength + 1))
lvui rt rs f s = (rs Bit..&. mask (f+s-1) s) `shift` (-f)

prop_Lvui :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Lvui = testLSvi lvui "lvuiTest"

prop_Lvuli :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Lvuli = testLSvi lvui "lvuliTest"

prop_Lvuri :: SubleqUWord -> SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Lvuri = testLSvi lvui "lvuriTest"

prop_Sbi  = testLSbi 3 svi  "sbiTest"
prop_Sbli = testLSbi 3 svi  "sbliTest"
prop_Sbri = testLSbi 3 svi  "sbriTest"
prop_Lbi  = testLSbi 3 lvui "lbuiTest"
prop_Lbli = testLSbi 3 lvui "lbuliTest"
prop_Lbri = testLSbi 3 lvui "lburiTest"

prop_Shi  = testLSbi 4 svi  "shiTest"
prop_Shli = testLSbi 4 svi  "shliTest"
prop_Shri = testLSbi 4 svi  "shriTest"
prop_Lhi  = testLSbi 4 lvui "lhuiTest"
prop_Lhli = testLSbi 4 lvui "lhuliTest"
prop_Lhri = testLSbi 4 lvui "lhuriTest"

prop_Addrw :: SubleqUWord -> SubleqUWord -> Bool
prop_Addrw rd rs = [rs'] == [rs] && rd' == rs `shift` (-2)
    where
      d = fromIntegral rd
      s = fromIntegral rs
      [rd', rs'] = map fromIntegral $ executeSubroutine "addrwTest" $ map fromIntegral [rd, rs]

prop_Addrb :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Addrb rd rt rs = [rs'] == [rs] && rt' == rs Bit..&. 0x3 && rd' == rs `shift` (-2)
    where
      d = fromIntegral rd
      t = fromIntegral rt
      s = fromIntegral rs
      [rd', rt', rs'] = map fromIntegral $ executeSubroutine "addrbTest" $ map fromIntegral [rd, rt, rs]

prop_Addrh :: SubleqUWord -> SubleqUWord -> SubleqUWord -> Bool
prop_Addrh rd rt rs = [rs'] == [rs] && rt' == (rs Bit..&. 0x2) `shift` (-1) && rd' == rs `shift` (-2)
    where
      d = fromIntegral rd
      t = fromIntegral rt
      s = fromIntegral rs
      [rd', rt', rs'] = map fromIntegral $ executeSubroutine "addrhTest" $ map fromIntegral [rd, rt, rs]

multD hi lo rs rt  = ([(iHi `shift` wordLength) + iLo, iHi `shift` wordLength, iHi, iLo, iRs, iRt], (iHi `shift` wordLength) + iLo == iRs * iRt)
    where
      iHi, iLo, iRs, iRt :: Integer
      iLo = fromIntegral (fromIntegral lo' :: SubleqUWord)
      [iHi, iRs, iRt] = map fromIntegral [hi', rs, rt]
      [hi', lo', _, _] = executeSubroutine "multD" [hi, lo, rs, rt]

showFix2SubleqState :: Fix2SubleqState -> Doc
showFix2SubleqState (pc, mem) = integer (fromIntegral pc) <> colon PP.<+> hsep registers PP.<+> colon PP.<+> hsep registersEx PP.<+> colon PP.<+> integer (memread 0x120 mem) PP.<+> colon PP.<+>  hsep (map integer [a,b,c,src1, src2])
    where
      memread a mem = fromIntegral $ Mem.read (fromIntegral a) mem
      registers = map (\a-> integer $ memread a mem) [0..15]
      registersEx = map (\a-> integer $ memread a mem) [16..31]
      [a, b, c] = map (`memread` mem) [pc..(pc+2)]
      [src1, src2] = map (`memread` mem) [a, b]

type SubleqResult a w m = Maybe (Maybe ([w], SubleqState a w m), [SubleqState a w m])
type Fix2SubleqResult = SubleqResult SubleqWord SubleqWord Fix2SubleqMemory

printExecution :: Fix2SubleqResult -> Doc
printExecution Nothing = text "Subroutine not found"
printExecution (Just (Nothing, ss)) = text "Non terminated" $$ vcat (take 50 $ map showFix2SubleqState ss)
printExecution r@(Just (Just (args, end), ss)) = status $$ history $$ result $$ insns $$ memoryDiff
    where
      status = text "Terminated"
      history = vcat (map showFix2SubleqState ss)
      result = text "result: " <> text (show args) <> semi PP.<+> hsep (map (text . printf "%x") args)
      insns = text "instructions: " <> integer (T.measureInsns r)
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

debugSubroutine :: A.Id -> [SubleqWord] -> Maybe Integer -> Doc
debugSubroutine i args tries = printExecution $ executeSubroutineWithStates i args tries

return []

res = T.res wordLength
randomSize = T.uniformTo wordLength

measureMultu n = do
    xs <- map floor <$> res n
    ys <- map floor <$> res n
    let xys = zip xs ys
    return $ do (x, y) <- xys
                let res = T.measureInsns $ executeSubroutineWithStates "multu" [0,0,x,y] maximumTry
                let ux = fromIntegral x :: SubleqUWord
                let uy = fromIntegral y :: SubleqUWord
                let pux = logBase 2 $ fromIntegral ux :: Double
                let puy = logBase 2 $ fromIntegral uy :: Double
                return (ux, uy, pux, puy, res)

measureShiftType sub n = do
    xs <- map floor <$> res n
    ys <- replicateM n $ R.sample (T.uniformTo wordLength)
    let xys = zip xs ys
    return $ do (x, y) <- xys
                let r' = executeSubroutineWithStates sub [0,x,y] maximumTry
                case r' of
                  Nothing -> error $ mconcat [toString sub, " is non terminate with ", show (x,y)]
                  Just _ -> return ()
                let res = T.measureInsns r'
                return (x, y, res)

measureSra n = do
    xs <- map floor <$> res n
    ys <- replicateM n $ R.sample randomSize
    let xys = zip xs ys
    return $ do (x, y) <- xys
                let res = T.measureInsns $ executeSubroutineWithStates "sra" [0,x,y] maximumTry
                return (x, y, res)

measureSrl n = do
    xs <- map floor <$> res n
    ys <- replicateM n $ R.sample randomSize
    let xys = zip xs ys
    return $ do (x, y) <- xys
                let res = T.measureInsns $ executeSubroutineWithStates "srl" [0,x,y] maximumTry
                return (x, y, res)

measureSviType sub n = do
    xs <- map floor <$> res n
    zs <- replicateM n $ R.sample randomSize
    ys <- forM zs (\ z -> R.sample (T.uniformTo (wordLength - z + 1)))
    let xys = zip3 xs ys zs
    return $ do (x, y, z) <- xys
                let res = T.measureInsns $ executeSubroutineWithStates sub [0,x,y,z] maximumTry
                return (x, y, z, res)

measureSbiType b sub n = do
    xs <- map floor <$> res n
    ys <- replicateM n $ R.sample (T.uniformTo (wordLength `div` (1 `shift` b :: SubleqWord) - 1))
    let xys = zip xs ys
    return $ do (x, y) <- xys
                let r' = executeSubroutineWithStates sub [0,x,y] maximumTry
                case r' of
                  Nothing -> error $ mconcat [toString sub, " is non terminate with ", show (x,y)]
                  Just _ -> return ()
                let res = T.measureInsns r'
                return (x, y, res)

measureSvi = measureSviType "sviTest"
measureSvli = measureSviType "svliTest"
measureSvri = measureSviType "svriTest"
measureLvui = measureSviType "lvuiTest"
measureLvuli = measureSviType "lvuliTest"
measureLvuri = measureSviType "lvuriTest"

measureAddrbType sub n = do
    xs <- map floor <$> res n
    return $ do (x) <- xs
                let r' = executeSubroutineWithStates sub [0,0,x] maximumTry
                case r' of
                  Nothing -> error $ mconcat [toString sub, " is non terminate with ", show x]
                  Just _ -> return ()
                let res = T.measureInsns r'
                return (x, res)

outputCsv :: CSV.ToRecord a => FilePath -> [BL.ByteString] -> [a] -> IO ()
outputCsv path header f = BL.writeFile path $ BL.concat [BL.intercalate "," header, "\n", CSV.encode f]

showModule = A.renderLoadPackResult $ A.loadModulePacked subleqMA subleqMATextSection subleqModule subleqMAInitialMem

main :: IO ()
main = do
    ok <- $quickCheckAll
    args <- getArgs
    print args
    unless ok $ putStrLn "Verification Failed!"
    when (args == ["measure"]) $ do
      let n = 10000
      -- putStrLn "Measure multu"
      outputCsv "measure-subleqr-multu.csv" ["arg1","arg2","parg1","parg2","insns"] =<< measureMultu n
      -- putStrLn "Measure sra"
      outputCsv "measure-subleqr-sra.csv" ["arg1","arg2","insns"] =<< measureSra n
      -- putStrLn "Measure srl"
      outputCsv "measure-subleqr-srl.csv" ["arg1","arg2","insns"] =<< measureSrl n
      measureType "" measureShiftType "sll" ["arg1","arg2","insns"] n
      measureType "" measureShiftType "srl" ["arg1","arg2","insns"] n
      measureType "" measureShiftType "sra" ["arg1","arg2","insns"] n
      measure "svi" measureSvi ["arg1","arg2","arg3","insns"] n
      measure "svli" measureSvli ["arg1","arg2","arg3","insns"] n
      measure "svri" measureSvri ["arg1","arg2","arg3","insns"] n
      measure "lvui" measureLvui ["arg1","arg2","arg3","insns"] n
      measure "lvuli" measureLvuli ["arg1","arg2","arg3","insns"] n
      measure "lvuri" measureLvuri ["arg1","arg2","arg3","insns"] n
      measureTest (measureSbiType 3) "sbi"   ["arg1","arg2","insns"] n
      measureTest (measureSbiType 3) "sbli"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 3) "sbri"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 3) "lbui"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 3) "lbuli" ["arg1","arg2","insns"] n
      measureTest (measureSbiType 3) "lburi" ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "shi"   ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "shli"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "shri"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "lhui"  ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "lhuli" ["arg1","arg2","insns"] n
      measureTest (measureSbiType 4) "lhuri" ["arg1","arg2","insns"] n
      measureTest measureAddrbType "addrb" ["arg1","insns"] n
      measureTest measureAddrbType "addrh" ["arg1","insns"] n
    case args of
      ("read-trace":fs) -> forM_ fs readTraceFromFile
      _ -> return ()
  where
    arch = "subleqr"
    measure name func cols n = do
      putStrLn $ "Measure " ++ toString name
      outputCsv (mconcat ["measure-", arch, "-", name, ".csv"]) cols =<< func n
    measureTest ty name = measure name (ty $ name `mappend` "Test")
    measureType suf ty name = measure name (ty $ name `mappend` suf)

readTraceFromFile :: FilePath -> IO ()
readTraceFromFile filename = do
    let outputfilename = FP.replaceBaseName filename ("measure-subleqr-" ++ FP.takeBaseName filename)
    f <- BL.readFile filename
    either (\x -> return ()) id $ (BL.writeFile outputfilename) . CSV.encode . M.toList <$> readTrace f

data SubleqArguments = LoadStore SubleqWord SubleqWord SubleqWord
    deriving (Show, Read, Eq, Ord)

parseTrace :: BL.ByteString -> Either String (Vector (String, SubleqArguments))
parseTrace = fmap (V.map conv) . CSV.decode CSV.NoHeader
    where
      conv (insn, offset, src1, src2, res_lw) = (insn, LoadStore (Prelude.read offset + Prelude.read src1) (Prelude.read src2) (Prelude.read res_lw))


traceExecute :: [(String, SubleqArguments)] -> [(String, Integer)]
traceExecute = map (\(insn, args) -> (insn, f insn args))

f :: String -> SubleqArguments -> Integer
f insn args = load (ncycle insn) args
load (addrRoutine, liRoutine, posF) (LoadStore addr rd mval) = 12 + measureAddr addrRoutine addr + nLwSub1 + measureLoad liRoutine (posF addr) mval rd
measureAddr sub addr = T.measureInsns $ executeSubroutineWithStates (sub ++ "Test") [0,0,addr] (Just 100)
measureLoad sub pos mval rd = T.measureInsns $ executeSubroutineWithStates (sub ++ "Test") [rd,mval,pos] (Just 100)
nLwSub1 = 6
nSwSub1 = 10
ncycle :: (Num a, Bits a, Integral a) => String -> (String, String, a -> a)
ncycle "lbu" = ("addrb", "lbui", \ n -> n `mod` 4)
ncycle "lb"  = ("addrb", "lbui", \ n -> n `mod` 4)
ncycle "sb"  = ("addrb", "sbi",  \ n -> n `mod` 4)
ncycle "lhu" = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "lh"  = ("addrh", "lhui", \ n -> (n `mod` 4) `shift` (-1))
ncycle "sh"  = ("addrh", "shi",  \ n -> (n `mod` 4) `shift` (-1))

readTrace :: BL.ByteString -> Either String (Map String Integer)
readTrace trace = M.fromListWith (+) . traceExecute . V.toList <$> parseTrace trace

