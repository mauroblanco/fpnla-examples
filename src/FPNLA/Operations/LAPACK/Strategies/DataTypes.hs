{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FPNLA.Operations.LAPACK.Strategies.DataTypes (

    HMatrixBindSeq,
    CholLLVBlocksSeq,
    CholLLVBlocksPar_Repa,
    CholLLVSeq,
    CholLLVPar_Repa,

    NullContext,

    SqrBlockContext,
    getSqrBlockDim,

    newNullContext,
    newSqrBlockContext,

) where


import           FPNLA.Operations.Parameters                (StratCtx)

-- TODO:Ver como sacar esto!!
import           FPNLA.Operations.BLAS.Strategies.DataTypes

data HMatrixBindSeq

data CholLLVSeq dots gemvs
data CholLLVPar_Repa dots gemvs

data CholLLVBlocksSeq potrfs syrks gemms trsms
data CholLLVBlocksPar_Repa potrfs syrks gemms trsms

type instance StratCtx HMatrixBindSeq = NullContext

type instance StratCtx (CholLLVSeq dots gemvs) = (StratCtx dots, StratCtx gemvs)
type instance StratCtx (CholLLVPar_Repa dots gemvs) = (StratCtx dots, StratCtx gemvs)

type instance StratCtx (CholLLVBlocksSeq syrks gemms trsms potrfs) =
    (SqrBlockContext, StratCtx syrks, StratCtx gemms, StratCtx trsms, StratCtx potrfs)
type instance StratCtx (CholLLVBlocksPar_Repa syrks gemms trsms potrfs) =
    (SqrBlockContext, StratCtx syrks, StratCtx gemms, StratCtx trsms, StratCtx potrfs)
