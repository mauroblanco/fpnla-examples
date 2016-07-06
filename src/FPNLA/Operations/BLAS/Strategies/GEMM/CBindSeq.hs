{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.CBindSeq () where

import           FPNLA.Matrix                               (Matrix,
                                                             MatrixVector,
                                                             cantRows_m)
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (CBindSeq)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             dimTrans_m,
                                                             unTransT)
import           FPNLA.Operations.Utils                     (matrixToPtr,
                                                             ptrToMatrix,
                                                             transToForeign,
                                                             unsafePerformIO)

import           Control.DeepSeq                            (NFData, rnf)
import           Control.Exception                          (evaluate)
import           Foreign.BLAS                               as L3 (BLAS3, Trans,
                                                                   gemm)
import           Foreign.Marshal.Alloc                      (free)

instance  (BLAS3 e, Elt e, NFData (m e), MatrixVector m v e) => GEMM CBindSeq m v e where
    gemm _ pmA pmB alfa beta mC =
        let ttA = transToForeign pmA
            ttB = transToForeign pmB
            mA = snd $ unTransT pmA
            mB = snd $ unTransT pmB
            (ldA, ldB, ldC) = (cantRows_m mA, cantRows_m mB, cantRows_m mC)
            (m, k, n) = (fst $ dimTrans_m pmA, snd $ dimTrans_m pmA, snd $ dimTrans_m pmB)
        in
            blasResultM $ ioGemm ttA ttB m n k alfa mA mB beta mC ldA ldB ldC

ioGemm :: (Matrix m a, Matrix m1 a,
             Matrix m2 a, Matrix m3 a, Elt a, BLAS3 a,
             NFData (m3 a)) =>
            Trans -> Trans -> Int -> Int -> Int
            -> a -> m a -> m1 a -> a -> m2 a 
            -> Int -> Int -> Int -> m3 a
ioGemm ttA ttB m n k alfa mA mB beta mC ldA ldB ldC= unsafePerformIO $ do
        mAptr <- matrixToPtr mA
        mBptr <- matrixToPtr mB
        mCptr <- matrixToPtr mC
        --L3.gemm  :: Trans -> Trans -> Int -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
        L3.gemm ttA ttB m n k alfa mAptr ldA mBptr ldB beta mCptr ldC
        res <- ptrToMatrix m n mCptr
        evaluate $ rnf res
        free mAptr
        free mBptr
        free mCptr
        return res
