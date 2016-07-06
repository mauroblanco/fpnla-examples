{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE FlexibleContexts #-} --TODO sacar al terminar de debugear junto con el Show

module FPNLA.Operations.LAPACK.Strategies.POTRF.Repa.BlocksPar (

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
import           FPNLA.Matrix.Instances.RepaMatrix            (RepaMatrix,
                                                               RepaVector)
import           FPNLA.Operations.BLAS                        (GEMM (gemm),
                                                               SYRK (syrk),
                                                               TRSM (trsm))
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (CholLLVBlocksPar_Repa,
                                                               SqrBlockContext,
                                                               getSqrBlockDim)
import           FPNLA.Operations.Parameters                  (Elt, ResM,
                                                               TransType (..),
                                                               TriangType (..),
                                                               UnitType (..),
                                                               blasResultM,
                                                               getResultDataM)
import           FPNLA.Utils                                  (iif)

import           Data.Array.Repa                              (Array, D, Shape,
                                                               U, deepSeqArray,
                                                               delay)
import           Data.Array.Repa.Eval                         (suspendedComputeP)
import           Data.Array.Repa.Repr.Unboxed                 (Unbox)

import           Control.DeepSeq                              (NFData)

--import Debug.Trace

--trace' s a = trace (s ++ ": " ++ (show a)) a

instance (Elt e, Unbox e, NFData e,
         MatrixVector RepaMatrix RepaVector e,
         POTRF potrfs RepaMatrix RepaVector e,
         SYRK syrks RepaMatrix RepaVector e,
         GEMM gemms RepaMatrix RepaVector e,
         TRSM trsms RepaMatrix RepaVector e) =>
         POTRF (CholLLVBlocksPar_Repa syrks gemms trsms potrfs) RepaMatrix RepaVector e where
    potrf ctx (Lower mA)
        =  blasResultM $ chol_blk_l ctx 0 mA (generate_m 0 0 undefined)
        where
            chol_blk_l ctx@(block_ctx, syrk_ctx, gemm_ctx, trsm_ctx, potrf_ctx) k mA mAL
                | k == 0           =  let  mA11'  = delay . computeUnboxedP $
                                                    call_chol_unb (Lower mA11)
                                           mA21'  = delay . computeUnboxedP .
                                                    deepSeqArray mA11' .
                                                    deepSeqArray mA21 $
                                                    transpose_m $ call_trsm 1 (NoTrans . Lower $ NoUnit mA11') (transpose_m mA21)
                                           mAx1   = delay . computeUnboxedP .
                                                    deepSeqArray mA21 $
                                                    iif (k == cantBlocks - 1) mA11' $ concatByCol_m mA11' mA21'
                                      in chol_blk_l ctx (k + 1) mA mAx1
                | k == cantBlocks  =  mAL
                | otherwise        =  chol_blk_l ctx (k + 1) mA mAL'

                where

                    {-# INLINE mA_dim #-}
                    mA_dim      = cantCols_m mA
                    {-# INLINE block #-}
                    block       = getSqrBlockDim block_ctx
                    {-# INLINE cantBlocks #-}
                    cantBlocks  = mA_dim `div` block

                    mA10        = delay . computeUnboxedP .
                                  deepSeqArray mAL $
                                  subMatrix_m (block * k) 0 block (block*k) mAL
                    mA11        = delay . computeUnboxedP $
                                  subMatrix_m (block * k) (block * k) block block mA
                    mA11'       = delay . computeUnboxedP .
                                  deepSeqArray mA10
                                  deepSeqArray mA11 $
                                  call_syrk (-1) (NoTrans mA10) 1 (Lower mA11)
                    mA11''      = delay . computeUnboxedP .
                                  deepSeqArray mA11' $
                                  call_chol_unb (Lower mA11')
                    mA20        = delay . computeUnboxedP $
                                  subMatrix_m ((k + 1) * block) 0 (mA_dim - (k + 1)*block) (k*block) mAL
                    mA21        = delay . computeUnboxedP $
                                  subMatrix_m ((k + 1) * block) (k * block) (mA_dim - (k + 1)*block) block mA
                    mA21'       = delay . computeUnboxedP .
                                  deepSeqArray mA20 .
                                  deepSeqArray mA21 $
                                  call_gemm  (NoTrans mA20) (Trans mA10) (-1) 1 mA21
                    mA21''      = delay . computeUnboxedP .
                                  transpose_m $ call_trsm 1 (NoTrans . Lower $ NoUnit mA11'') (transpose_m mA21')

                    mAx1        = delay . computeUnboxedP .
                                  add_zeros k block . iif (k == cantBlocks - 1) mA11'' $ concatByCol_m mA11'' mA21''

                    mAL'        = delay . computeUnboxedP .
                                  fromCols_vm $ (toCols_vm mAL :: [RepaVector e]) ++ (toCols_vm mAx1 :: [RepaVector e])

                    call_syrk !m1 !alpha !beta !m2 = getResultDataM $ (syrk syrk_ctx m1 alpha beta m2 :: ResM syrks RepaVector RepaMatrix e)
                    call_chol_unb !m = getResultDataM $ (potrf potrf_ctx m :: ResM potrfs RepaVector RepaMatrix e)
                    call_gemm !m1 !m2 !alpha !beta !m3 = getResultDataM $ (gemm gemm_ctx m1 m2 alpha beta m3 :: ResM gemms RepaVector RepaMatrix e)
                    call_trsm !alpha !m1 !m2 = getResultDataM $ (trsm trsm_ctx alpha m1 m2 :: ResM trsms RepaVector RepaMatrix e)

                    add_zeros :: (Matrix RepaMatrix e) => Int -> Int -> RepaMatrix e -> RepaMatrix e
                    add_zeros !k !block !v = concatByCol_m (generate_m (k*block) (block) (\_ _ -> 0) :: RepaMatrix e) v

concatByCol_m :: (Unbox e, Matrix RepaMatrix e) => RepaMatrix e -> RepaMatrix e -> RepaMatrix e
concatByCol_m !m1 !m2 = let rows_m1 = cantRows_m m1
                            cols_m1 = cantCols_m m1
                            rows_m2 = cantRows_m m2
                            cols_m2 = cantCols_m m2
                        in generate_m (rows_m1 + rows_m2) cols_m1
                                      (\i j -> iif (i >= rows_m1) (elem_m (i - rows_m1) j m2)
                                                                  (elem_m i j m1))

computeUnboxedP :: (Unbox e, Shape sh) => Array D sh e -> Array U sh e
computeUnboxedP = suspendedComputeP
