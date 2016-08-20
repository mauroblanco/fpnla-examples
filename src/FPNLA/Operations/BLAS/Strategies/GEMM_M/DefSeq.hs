{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM_M.DefSeq () where

import           FPNLA.Matrix_M
import           FPNLA.Operations.BLAS_M
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefSeq)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             TransType(..))

import Control.Exception
import Control.Monad (foldM)

--import Debug.Trace (trace)

instance (Elt e, Matrix IO m e, Vector IO v e, RowMatrixVector IO m v e) => GEMM_M IO DefSeq m v e where
    gemm_m _ (NoTrans mA) (Trans mB) alpha beta mC =
        do
            (_, _) <- dim_m mA
            (_, p) <- dim_m mB
            let matMultIJ i j =
                    foldM (\r x ->
                        do
                            maIX <- elem_m i x mA
                            mbXJ <- elem_m j x mB
                            let res = r + maIX * mbXJ
                            _ <- evaluate res
                            return res) 0 [0 .. p - 1]
            blasResultM <$> update_m (\i j ->
                do
                    multIJ <- matMultIJ i j
                    mcIJ <- elem_m i j mC
                    return $ alpha * multIJ + beta * mcIJ) mC
    gemm_m _ _ _ _ _ _ = error "TODO!"
