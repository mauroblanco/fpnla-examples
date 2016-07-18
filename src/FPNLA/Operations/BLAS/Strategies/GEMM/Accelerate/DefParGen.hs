{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.Accelerate.DefParGen () where

import           FPNLA.Matrix                               (MatrixVector (),
                                                             elem_m, fromList_m, dim_m)
import           FPNLA.Matrix.Instances.AccMatrix           (AccMatrix (..),
                                                             AccVector (..))
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefPar_AccGen, DefPar_Acc)
import           FPNLA.Operations.BLAS.Strategies.GEMM.Accelerate.DefPar()
import           FPNLA.Operations.Parameters                (ResM,
                                                             blasResultM,
                                                             getResultDataM,
                                                             Elt, unTransT)

import           Data.Array.Accelerate                      (IsNum, toList)
import qualified Data.Array.Accelerate                      as A (Elt)
import           Data.Array.Accelerate.Interpreter as R (run)

instance (Elt e, MatrixVector m v e, A.Elt e, IsNum e) => GEMM DefPar_AccGen m v e where
    gemm sctx pmA pmB alpha beta mC = blasResultM $ fromList_m rowsC colsC listResultData
        where --callGemm mA mB mC = undefined
              (transTA, mA) = unTransT pmA
              (transTB, mB) = unTransT pmB
              (rowsA, colsA) = dim_m mA
              (rowsB, colsB) = dim_m mB
              (rowsC, colsC) = dim_m mC
              mA' :: (AccMatrix e) = fromList_m rowsA colsA
                                                [elem_m i j mA | i <- [0 .. (rowsA - 1)], j <- [0 .. (colsA - 1)]]
              mB' :: (AccMatrix e) = fromList_m rowsB colsB
                                                [elem_m i j mB | i <- [0 .. (rowsB - 1)], j <- [0 .. (colsB - 1)]]
              mC' :: (AccMatrix e) = fromList_m rowsC colsC
                                                [elem_m i j mC | i <- [0 .. (rowsC - 1)], j <- [0 .. (colsC - 1)]]
              accResultData = getResultDataM (gemm sctx (transTA mA') (transTB mB') alpha beta mC' :: ResM DefPar_Acc AccVector AccMatrix e)
              listResultData = toList . run . (\(AccMatrix m) -> m) $ accResultData
