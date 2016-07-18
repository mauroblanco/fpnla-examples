{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.LAPACK.Strategies.POTRF.BlocksSeq (

) where


import           FPNLA.Matrix                                 (Matrix,
                                                               MatrixVector,
                                                               cantCols_m,
                                                               cantRows_m,
                                                               elem_m,
                                                               fromCols_vm,
                                                               generate_m,
                                                               subMatrix_m,
                                                               toCols_vm,
                                                               transpose_m)
import           FPNLA.Operations.BLAS                        (GEMM (gemm),
                                                               SYRK (syrk),
                                                               TRSM (trsm))
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (CholLLVBlocksSeq,
                                                               getSqrBlockDim)
import           FPNLA.Operations.Parameters                  (Elt, ResM,
                                                               TransType (..),
                                                               TriangType (..),
                                                               UnitType (..),
                                                               blasResultM,
                                                               getResultDataM)
import           FPNLA.Utils                                  (iif)

import           Debug.Trace                                  (trace)

instance (Elt e, MatrixVector m v e, POTRF potrfs m v e, SYRK syrks m v e,
         GEMM gemms m v e, TRSM trsms m v e) =>
         POTRF (CholLLVBlocksSeq syrks gemms trsms potrfs) m v e where
    potrf _ (Upper _) = trace "potrf" undefined
    potrf ctx (Lower mA)
        =  blasResultM $ chol_blk_l ctx 0 mA (generate_m 0 0 undefined)
        where
            chol_blk_l ctx@(block_ctx, syrk_ctx, gemm_ctx, trsm_ctx, potrf_ctx) k mA mAL
                | k == 0           =  let  mA11'  = call_chol_unb (Lower mA11)
                                           mA21'  = transpose_m $ call_trsm 1 (NoTrans . Lower $ NoUnit mA11') (transpose_m mA21)
                                           mAx1   = iif (k == cantBlocks - 1) mA11' $ concatByCol_m mA11' mA21'
                                      in chol_blk_l ctx (k + 1) mA mAx1
                | k == cantBlocks  =  mAL
                | otherwise        =  chol_blk_l ctx (k + 1) mA mAL'

                where
                    mA_dim      = cantCols_m mA
                    mA10        = subMatrix_m (block * k) 0 block (block*k) mAL
                    mA11        = subMatrix_m (block * k) (block * k) block block mA
                    mA11'       = call_syrk (-1) (NoTrans mA10) 1 (Lower mA11)
                    mA11''      = call_chol_unb (Lower mA11')
                    mA20        = subMatrix_m ((k + 1) * block) 0 (mA_dim - (k + 1)*block) (k*block) mAL
                    mA21        = subMatrix_m ((k + 1) * block) (k * block) (mA_dim - (k + 1)*block) block mA
                    mA21'       = call_gemm  (NoTrans mA20) (Trans mA10) (-1) 1 mA21
                    mA21''      = transpose_m $ call_trsm 1 (NoTrans . Lower $ NoUnit mA11'') (transpose_m mA21')

                    mAx1        = add_zeros k block . iif (k == cantBlocks - 1) mA11'' $ concatByCol_m mA11'' mA21''

                    mAL'        = fromCols_vm $ (toCols_vm mAL :: [v e]) ++ (toCols_vm mAx1 :: [v e])

                    call_syrk m1 alpha beta m2 = getResultDataM (syrk syrk_ctx m1 alpha beta m2 :: ResM syrks v m e)
                    call_chol_unb m = getResultDataM (potrf potrf_ctx m :: ResM potrfs v m e)
                    call_gemm m1 m2 alpha beta m3 = getResultDataM (gemm gemm_ctx m1 m2 alpha beta m3 :: ResM gemms v m e)
                    call_trsm alpha m1 m2 = getResultDataM (trsm trsm_ctx alpha m1 m2 :: ResM trsms v m e)

                    add_zeros :: (Matrix m e) => Int -> Int -> m e -> m e
                    add_zeros k block = concatByCol_m (generate_m (k*block) block (\_ _ -> 0) :: m e)

                    block       = getSqrBlockDim block_ctx
                    cantBlocks  = mA_dim `div` block

concatByCol_m :: (Matrix m e) => m e -> m e -> m e
concatByCol_m m1 m2 = let rows_m1 = cantRows_m m1
                          cols_m1 = cantCols_m m1
                          rows_m2 = cantRows_m m2
                      in generate_m (rows_m1 + rows_m2) cols_m1
                                    (\i j -> iif (i >= rows_m1) (elem_m (i - rows_m1) j m2)
                                                                (elem_m i j m1))

