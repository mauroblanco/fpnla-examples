{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.TRSM.CBindSeq () where

import           FPNLA.Matrix                               (Matrix, MatrixVector,
                                                             cantCols_m,
                                                             cantRows_m)
import           FPNLA.Operations.BLAS                      (TRSM(..))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (CBindSeq)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             unTransT,
                                                             unTriangT, unUnitT)
import           FPNLA.Operations.Utils                     (matrixToPtr,
                                                             ptrToMatrix,
                                                             transToForeign,
                                                             triangToForeign,
                                                             unitToForeign,
                                                             unsafePerformIO)


import           Control.DeepSeq                            (NFData (rnf))
import           Control.Exception                          (evaluate)
import           Foreign.BLAS                               as L3 (BLAS3, Side (LeftSide),
                                                                   trsm, Uplo, Trans, Diag)
import           Foreign.Marshal.Alloc                      (free)

instance (BLAS3 e, Elt e, NFData (m e), MatrixVector m v e) => TRSM CBindSeq m v e where
    trsm _ alpha pmA mB =
        let
            (_, pmA') = unTransT pmA
            (_, pmA'') = unTriangT pmA'
            (_, mA) = unUnitT pmA''
            trans = transToForeign pmA
            uplo = triangToForeign pmA'
            diag = unitToForeign pmA''
            (m, n) = (cantRows_m mA, cantCols_m mB)
            (ldA, ldB) = (cantRows_m mA, cantRows_m mB)
        in
            blasResultM $ ioTrsm uplo trans diag m n alpha mA mB ldA ldB

ioTrsm :: (Matrix m a, Matrix m1 a,
           Matrix m2 a, Elt a, BLAS3 a, NFData (m2 a)) =>
           Uplo -> Trans -> Diag -> Int -> Int
           -> a -> m a -> m1 a
           -> Int -> Int -> m2 a
ioTrsm uplo trans diag m n alpha mA mB ldA ldB = unsafePerformIO $ do
        mAptr <- matrixToPtr mA
        mBptr <- matrixToPtr mB
        --L3.trsm  :: Side -> Uplo -> Trans -> Diag -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()
        L3.trsm LeftSide uplo trans diag m n alpha mAptr ldA mBptr ldB
        res <- ptrToMatrix m n mBptr
        evaluate $ rnf res
        free mAptr
        free mBptr
        return res
