{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Main(main) where

import Test.QuickCheck (Arbitrary,quickCheckResult)
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Default (Default(def))

import qualified Data.Packed.Matrix as HM (Matrix)
import qualified Data.Packed.Vector as HV (Vector)

import TestBase (Truncable, GEMMTestData(..), StrassenTestData(..), TRSMTestData(..), Tag2(), eqMatrix, handleResult)

import FPNLA.Matrix.Instances.HMatrix ()
import FPNLA.Matrix.Instances.AccMatrix (AccMatrix, AccVector)
import Data.Array.Repa.Repr.Unboxed ()
import FPNLA.Operations.BLAS (GEMM(gemm), TRSM(trsm))
import FPNLA.Operations.Parameters (StratCtx, ResM, getResultDataM)
import FPNLA.Operations.BLAS.Strategies (CBindSeq, DefSeq, BlocksSeq, BlocksByRows, BlocksByCols, DefPar_MP, BlocksPar_MP, StrassenPar_MP, ColsPar_MP, DefPar_ST, DefPar_Acc, BlocksPar_ST, StrassenPar_ST, ColsPar_ST)

import qualified Data.Array.Accelerate as A (
    IsNum, Elt)


-- Versión general de parar realizar pruebas de GEMM:
propGEMMStratIsOK :: forall st m v e. (Show e, Eq e, Truncable e, Arbitrary e, GEMM st m v e, Default (Tagged st (StratCtx st)), GEMM CBindSeq m v e) => GEMMTestData m e -> Tagged (Tag2 st v) Bool
propGEMMStratIsOK (GEMMTestData pmA pmB alpha beta mC) = Tagged $ eqMatrix r1 r2
    where
        r1 = getResultDataM (gemm (unTagged (def :: Tagged st (StratCtx st))) pmA pmB alpha beta mC :: ResM st v m e)
        r2 = getResultDataM (gemm (unTagged (def :: Tagged CBindSeq (StratCtx CBindSeq))) pmA pmB alpha beta mC :: ResM CBindSeq v m e)

propGEMMAccStratIsOK :: forall st e. (A.Elt e, A.IsNum e, Eq e, Truncable e, Arbitrary e, GEMM st AccMatrix AccVector e, Default (Tagged st (StratCtx st)), GEMM CBindSeq AccMatrix AccVector e) => GEMMTestData AccMatrix e -> Tagged (Tag2 st AccVector) Bool
propGEMMAccStratIsOK (GEMMTestData pmA pmB alpha beta mC) = Tagged $ eqMatrix r1 r2
    where
        r1 = getResultDataM (gemm (unTagged (def :: Tagged st (StratCtx st))) pmA pmB alpha beta mC :: ResM st AccVector AccMatrix e)
        r2 = getResultDataM (gemm (unTagged (def :: Tagged CBindSeq (StratCtx CBindSeq))) pmA pmB alpha beta mC :: ResM CBindSeq AccVector AccMatrix e)


propStrassenStratIsOK :: forall st m v e. (Show e, Eq e, Truncable e, Arbitrary e, GEMM st m v e, Default (Tagged st (StratCtx st)), GEMM CBindSeq m v e) => StrassenTestData m e -> Tagged (Tag2 st v) Bool
propStrassenStratIsOK (StrassenTestData pmA pmB alpha beta mC) = Tagged $ eqMatrix r1 r2
    where
        r1 = getResultDataM (gemm (unTagged (def :: Tagged st (StratCtx st))) pmA pmB alpha beta mC :: ResM st v m e)
        r2 = getResultDataM (gemm (unTagged (def :: Tagged CBindSeq (StratCtx CBindSeq))) pmA pmB alpha beta mC :: ResM CBindSeq v m e)

propTRSMStratIsOK :: forall st m v e. (Show e, Eq e, Truncable e, Arbitrary e, TRSM st m v e, Default (Tagged st (StratCtx st)), TRSM CBindSeq m v e) => TRSMTestData m e -> Tagged (Tag2 st v) Bool
propTRSMStratIsOK (TRSMTestData alpha mA mB) = Tagged $ eqMatrix r1 r2
    where
        r1 = getResultDataM (trsm (unTagged (def :: Tagged st (StratCtx st))) alpha mA mB :: ResM st v m e)
        r2 = getResultDataM (trsm (unTagged (def :: Tagged CBindSeq (StratCtx CBindSeq))) alpha mA mB :: ResM CBindSeq v m e)

--
type TMatrix = {-VMatrix-} {--VMatrix--} HM.Matrix {-LLR.LLMatrix-} {-RepaMatrix-}
type TVector = {-VVector-} {--A.Array Int--} HV.Vector {-LLR.LVector-} {-RepaVector-}
type TElem = Double

propDefSeqStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 DefSeq TVector) Bool
propDefSeqStratIsOK_Inst = propGEMMStratIsOK
propBlocksSeqStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 (BlocksSeq DefSeq) TVector) Bool
propBlocksSeqStratIsOK_Inst = propGEMMStratIsOK
propTRSMStratIsOK_Inst :: TRSMTestData TMatrix TElem -> Tagged (Tag2 DefSeq TVector) Bool
propTRSMStratIsOK_Inst = propTRSMStratIsOK
propTRSMBlockByColsIsOK_Inst :: TRSMTestData TMatrix TElem -> Tagged (Tag2 (BlocksByCols DefSeq DefSeq) TVector) Bool
propTRSMBlockByColsIsOK_Inst = propTRSMStratIsOK
propTRSMBlockByRowsIsOK_Inst :: TRSMTestData TMatrix TElem -> Tagged (Tag2 (BlocksByRows DefSeq DefSeq) TVector) Bool
propTRSMBlockByRowsIsOK_Inst = propTRSMStratIsOK

propDefParMPStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 DefPar_MP TVector) Bool
propDefParMPStratIsOK_Inst = propGEMMStratIsOK
propBlocksParMPStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 (BlocksPar_MP DefSeq) TVector) Bool
propBlocksParMPStratIsOK_Inst = propGEMMStratIsOK
propStrassenMPStratIsOK_Inst :: StrassenTestData TMatrix TElem -> Tagged (Tag2 (StrassenPar_MP DefSeq) TVector) Bool
propStrassenMPStratIsOK_Inst = propStrassenStratIsOK
propTRSMMPStratIsOK_Inst :: TRSMTestData TMatrix TElem -> Tagged (Tag2 (ColsPar_MP DefSeq) TVector) Bool
propTRSMMPStratIsOK_Inst = propTRSMStratIsOK

propDefParSTStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 DefPar_ST TVector) Bool
propDefParSTStratIsOK_Inst = propGEMMStratIsOK
propBlocksParSTStratIsOK_Inst :: GEMMTestData TMatrix TElem -> Tagged (Tag2 (BlocksPar_ST DefSeq) TVector) Bool
propBlocksParSTStratIsOK_Inst = propGEMMStratIsOK
propStrassenSTStratIsOK_Inst :: StrassenTestData TMatrix TElem -> Tagged (Tag2 (StrassenPar_ST DefSeq) TVector) Bool
propStrassenSTStratIsOK_Inst = propStrassenStratIsOK
propTRSMSTStratIsOK_Inst :: TRSMTestData TMatrix TElem -> Tagged (Tag2 (ColsPar_ST DefSeq) TVector) Bool
propTRSMSTStratIsOK_Inst = propTRSMStratIsOK

propDefParAccStratIsOK_Inst :: GEMMTestData AccMatrix TElem -> Tagged (Tag2 DefPar_Acc AccVector) Bool
propDefParAccStratIsOK_Inst = propGEMMAccStratIsOK

main :: IO ()
main = do
    putStrLn "execGEMMQC"
    res1 <- quickCheckResult propDefSeqStratIsOK_Inst
    res2 <- quickCheckResult propBlocksSeqStratIsOK_Inst
    res3 <- quickCheckResult propDefParMPStratIsOK_Inst
    res4 <- quickCheckResult propBlocksParMPStratIsOK_Inst
    res5 <- quickCheckResult propDefParSTStratIsOK_Inst
    res6 <- quickCheckResult propBlocksParSTStratIsOK_Inst
    putStrLn "execGEMMAccQC"
    --res7 <- quickCheckResult propDefParAccStratIsOK_Inst
    putStrLn "execStrassenQC"
    res8 <- quickCheckResult propStrassenMPStratIsOK_Inst
    res10 <- quickCheckResult propStrassenSTStratIsOK_Inst
    putStrLn "execTRSMQC"
    res11 <- quickCheckResult propTRSMStratIsOK_Inst
    res12 <- quickCheckResult propTRSMBlockByColsIsOK_Inst
    res13 <- quickCheckResult propTRSMBlockByRowsIsOK_Inst
    res14 <- quickCheckResult propTRSMMPStratIsOK_Inst
    res15 <- quickCheckResult propTRSMSTStratIsOK_Inst
    handleResult res1
    handleResult res2
    handleResult res3
    handleResult res4
    handleResult res5
    handleResult res6
    --handleResult res7
    handleResult res8
    handleResult res10
    handleResult res11
    handleResult res12
    handleResult res13
    handleResult res14
    handleResult res15
    
    

