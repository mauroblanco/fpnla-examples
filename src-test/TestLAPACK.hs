{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Main (

    main,

) where

import Test.QuickCheck (Arbitrary, quickCheckResult)
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Default (Default(def))
import System.Random (Random)

--import qualified Data.Array as A (Array)
--import qualified Data.Array.Repa as R (D, DIM1, DIM2, Array)
import qualified Data.Packed.Matrix as HM (Matrix)
import qualified Data.Packed.Vector as HV (Vector)

import TestBase (Truncable, PotrfTestData(..), Tag2(), eqMatrix, handleResult)

import FPNLA.Matrix.Instances.RepaMatrix(RepaMatrix, RepaVector)
import FPNLA.Matrix.Instances.HMatrix ()
import FPNLA.Operations.Parameters (ResM, getResultDataM, StratCtx)
import FPNLA.Operations.LAPACK.Strategies (HMatrixBindSeq, CholLLVSeq, CholLLVPar_Repa, CholLLVBlocksSeq)
import FPNLA.Operations.BLAS.Strategies (DefSeq, DefPar_ST)
import FPNLA.Operations.LAPACK (POTRF(potrf))

-- Propiedades:
propPotrfIsOK :: forall st m v e. (Show e, Eq e, Truncable e, Arbitrary e, Random e, POTRF st m v e, Default (Tagged st (StratCtx st)), POTRF HMatrixBindSeq m v e) => PotrfTestData m e -> Tagged (Tag2 st v) Bool
propPotrfIsOK (PotrfTestData pmA) = Tagged $ eqMatrix r1 r2
    where
        r1 = getResultDataM (potrf (unTagged (def :: Tagged st (StratCtx st))) pmA :: ResM st v m e)
        r2 = getResultDataM (potrf (unTagged (def :: Tagged HMatrixBindSeq (StratCtx HMatrixBindSeq))) pmA :: ResM HMatrixBindSeq v m e)

type TMatrix = {-VMatrix-} {--VMatrix--} HM.Matrix {-LLR.LLMatrix-} {-RepaMatrix-}
type TVector = {-VVector-} {--A.Array Int--} HV.Vector {-LLR.LVector-} {-RepaVector-}
--type TElem = Complex Double
type TElem = Double

propTest :: PotrfTestData HM.Matrix Double -> Tagged (Tag2 HMatrixBindSeq HV.Vector) Bool
propTest = propPotrfIsOK

propPotrfDefSeqStratIsOK_Inst :: PotrfTestData TMatrix TElem -> Tagged (Tag2 (CholLLVSeq DefSeq DefSeq) TVector) Bool
propPotrfDefSeqStratIsOK_Inst = propPotrfIsOK

propPotrfRepaDefSeqStratIsOK_Inst :: PotrfTestData RepaMatrix TElem -> Tagged (Tag2 (CholLLVPar_Repa DefSeq DefSeq) RepaVector) Bool
propPotrfRepaDefSeqStratIsOK_Inst = propPotrfIsOK

propPotrfCholLLVBlocksSeqStratIsOK_Inst :: PotrfTestData TMatrix TElem -> Tagged (Tag2 (CholLLVBlocksSeq DefPar_ST DefSeq DefSeq (CholLLVSeq DefSeq DefSeq)) TVector) Bool
propPotrfCholLLVBlocksSeqStratIsOK_Inst = propPotrfIsOK

execPotrfQC :: IO [()]
execPotrfQC = do
    putStrLn "execPotrfQC"
    res1 <- quickCheckResult propTest
    res2 <- quickCheckResult propPotrfDefSeqStratIsOK_Inst
    res3 <- quickCheckResult propPotrfRepaDefSeqStratIsOK_Inst
    res4 <- quickCheckResult propPotrfCholLLVBlocksSeqStratIsOK_Inst
    mapM handleResult [res1, res2, res3, res4]

execQuickCheck :: IO [()]
execQuickCheck = execPotrfQC

main :: IO [()]
main = execQuickCheck

