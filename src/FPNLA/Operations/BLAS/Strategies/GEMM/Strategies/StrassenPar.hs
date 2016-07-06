{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.StrassenPar () where

import           Control.DeepSeq                            (NFData)
import           Control.Parallel.Strategies                (parTraversable,
                                                             rdeepseq,
                                                             withStrategy)
import           FPNLA.Matrix                               (cantCols_m,
                                                             cantRows_m,
                                                             fromBlocks_m,
                                                             fromBlocks_m,
                                                             generate_m,
                                                             toBlocks_m,
                                                             zipWith_m)
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (StrassenPar_ST,
                                                             getStrassenLimit)
import           FPNLA.Operations.Parameters                (Elt, ResM, TransType (NoTrans),
                                                             blasResultM,
                                                             getResultDataM,
                                                             transTrans_m)

instance (NFData (m e), Elt e, GEMM s m v e) => GEMM (StrassenPar_ST s) m v e where
    gemm (ctx, sctx) pmA pmB alpha beta mC =
        blasResultM $ strassen mA mB alpha beta mC
        where

            callGemm mA mB alpha beta mC = getResultDataM (gemm sctx (NoTrans mA) (NoTrans mB) alpha beta mC :: ResM s v m e)

            mA = transTrans_m pmA
            mB = transTrans_m pmB

            strassenLimit = getStrassenLimit ctx

            strassen :: m e -> m e -> e -> e -> m e -> m e
            strassen mA mB alpha beta mC =
                if cantRows_m mA <= strassenLimit
                then callGemm mA mB alpha beta mC
                else quadJoin . toC . toM $ (quadPartition mA, quadPartition mB, quadPartition mC)

            quadPartition :: m e -> (m e, m e, m e, m e)
            quadPartition m =
                let
                blocks = toBlocks_m (div (cantRows_m m) 2) (div (cantCols_m m) 2) m in
                (head (head blocks), head blocks !! 1, head (blocks !! 1), blocks !! 1 !! 1)

            fillWithZeros :: m e -> m e
            fillWithZeros m = generate_m (cantRows_m m) (cantCols_m m) (\_ _ -> 0)

            toM :: ((m e, m e, m e, m e), (m e, m e, m e, m e), (m e, m e, m e, m e)) -> (m e, m e, m e, m e, m e, m e, m e)
            toM ((a11, a12, a21, a22), (b11, b12, b21, b22), (c11, c12, c21, c22)) =
                (\[a, b, c, d, e, f, g] -> (a, b, c, d, e, f, g)) . withStrategy (parTraversable rdeepseq) $
                    [strassen (zipWith_m (+) a11 a22) (zipWith_m (+) b11 b22) alpha 0 (fillWithZeros c11),
                    strassen (zipWith_m (+) a21 a22) b11 alpha 0 (fillWithZeros c11),
                    strassen a11 (zipWith_m (-) b12 b22) alpha beta c12,
                    strassen a22 (zipWith_m (-) b21 b11) alpha beta c21,
                    strassen (zipWith_m (+) a11 a12) b22 alpha 0 (fillWithZeros c11),
                    strassen (zipWith_m (-) a21 a11) (zipWith_m (+) b11 b12) alpha beta (zipWith_m (-) c22 c12),
                    strassen (zipWith_m (-) a12 a22) (zipWith_m (+) b21 b22) alpha beta (zipWith_m (-) c11 c21)]

            toC :: (m e, m e, m e, m e, m e, m e, m e) -> (m e, m e, m e, m e)
            toC (m1, m2, m3, m4, m5, m6, m7) =
                (\[a, b, c, d] -> (a, b, c, d)) . withStrategy (parTraversable rdeepseq) $
                    [zipWith_m (+) m1 . zipWith_m (+) m4 . zipWith_m (-) m7 $ m5,
                    zipWith_m (+) m3 m5,
                    zipWith_m (+) m2 m4,
                    zipWith_m (+) m1 . zipWith_m (+) m3 . zipWith_m (-) m6 $ m2]

            quadJoin :: (m e, m e, m e, m e) -> m e
            quadJoin (c11, c12, c21, c22) =
                fromBlocks_m [
                    [c11, c12],
                    [c21, c22]]
