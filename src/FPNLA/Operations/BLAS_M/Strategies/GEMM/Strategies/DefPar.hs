{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS_M.Strategies.GEMM.Strategies.DefPar () where


import           FPNLA.Matrix_M                                (Matrix(dim_m, elem_m),
                                                               Vector(update_v),
                                                               RowMatrixVector(toRows_vm))
import           FPNLA.Operations.BLAS_M                       (GEMM_M(gemm_m))
import           FPNLA.Operations.BLAS_M.Strategies.GEMM.Utils (asTrans, asNoTrans)
import           FPNLA.Operations.BLAS.Strategies.DataTypes    (DefPar_ST)
import           FPNLA.Operations.Parameters                   (Elt, blasResultM,
                                                               TransType(..))

import           Control.DeepSeq                               (rnf)
import           Control.Exception                             (evaluate)
import           Control.Monad                                 (foldM)
import           Control.Parallel.Strategies                   (parMap, rdeepseq, NFData)
import           System.IO.Unsafe                              (unsafePerformIO)

--import Debug.Trace (trace)

instance (NFData e, Elt e, Matrix IO m e, Vector IO v e, RowMatrixVector IO m v e) =>
    GEMM_M IO DefPar_ST m v e where
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
                            _ <- evaluate . rnf $ res
                            return res) 0 [0 .. p - 1]
            rows :: [v e] <- toRows_vm mC
            let f (row, i) = do
                    _ <- update_v (\j ->
                        do
                            multIJ <- matMultIJ i j
                            mcIJ <- elem_m i j mC
                            let res = alpha * multIJ + beta * mcIJ
                            _ <- evaluate . rnf $ res
                            return res) row
                    return ()
            let parResutl = parMap rdeepseq (unsafePerformIO . f) $ zip rows [0 .. ]
            _ <- evaluate . rnf $ parResutl
            return $ blasResultM mC
    gemm_m ctx mtA mtB alpha beta mC =
        do
            mA <- asNoTrans mtA
            mB <- asTrans mtB
            gemm_m ctx (NoTrans mA) (Trans mB) alpha beta mC


