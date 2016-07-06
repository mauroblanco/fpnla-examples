{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE FlexibleContexts #-} --TODO sacar al terminar de debugear junto con el Show

module FPNLA.Operations.LAPACK.Strategies.POTRF.Repa.DefPar (

) where

import           FPNLA.Matrix                                 (MatrixVector,
                                                               cantCols_m,
                                                               concat_v, elem_m,

                                                               fromCols_vm,
                                                               fromList_v,
                                                               generate_m,
                                                               generate_v,
                                                               map_v,
                                                               subMatrix_m,
                                                               toCols_vm)
import           FPNLA.Matrix.Instances.RepaMatrix            (RepaMatrix,
                                                               RepaVector)
import           FPNLA.Operations.BLAS                        (DOT (dot),
                                                               GEMV (gemv))
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (CholLLVPar_Repa)
import           FPNLA.Operations.Parameters                  (Elt, ResS, ResV,
                                                               TransType (..),
                                                               TriangType (..),
                                                               blasResultM,
                                                               getConjugate,
                                                               getResultDataS,
                                                               getResultDataV)
import           FPNLA.Utils                                  (iif)

import           Data.Array.Repa                              (Array, D, Shape,
                                                               U, deepSeqArray,
                                                               delay)
import           Data.Array.Repa.Eval                         (suspendedComputeP)
import           Data.Array.Repa.Repr.Unboxed                 (Unbox)

import           Control.DeepSeq                              (NFData, deepseq)

--import Debug.Trace

--trace' s a = trace (s ++ ": " ++ (show a)) a

instance (Elt e, Unbox e, NFData e,
          MatrixVector RepaMatrix RepaVector e,
          DOT dots RepaVector e,
          GEMV gemvs RepaMatrix RepaVector e) =>
          POTRF (CholLLVPar_Repa dots gemvs) RepaMatrix RepaVector e where
    potrf (dot_ctx, gemv_ctx) (Lower mA)
        =  blasResultM $ chol_l gemv_ctx 0 mA (generate_m 0 0 undefined)
        where
            chol_l gemv_ctx k mA mAL
                | k == 0  =  let {-# INLINE eA11' #-}
                                 eA11'  = sqrt eA11
                                 vA11'  = fromList_v [eA11']
                                 vA21'  =
                                          map_v (/eA11') mA21
                                 vAx1   =
                                          iif (k == mA_dim - 1) vA11' (concat_v [vA11', vA21'])
                                 mAL'   =
                                          fromCols_vm [vAx1]
                             in chol_l gemv_ctx (k + 1) mA mAL'
                | k == mA_dim  =  mAL
                | otherwise    =  chol_l gemv_ctx (k + 1) mA mAL'

                where
                    vA10    =  delay . computeUnboxedP .
                               deepSeqArray mAL $
                               generate_v k (\j -> elem_m k j mAL)
                    vA10_c  =
                               map_v getConjugate vA10
                    {-# INLINE vA10_p #-}
                    vA10_p  =  deepSeqArray vA10_c $
                               getResultDataS (dot dot_ctx vA10 vA10_c :: ResS dots e)
                    {-# INLINE eA11 #-}
                    eA11    =  elem_m k k mA
                    {-# INLINE eA11' #-}
                    eA11'   =  sqrt $ eA11 - vA10_p
                    vA11'   =  delay . computeUnboxedP .
                               deepseq eA11' $
                               fromList_v [eA11']
                    mA21    =  delay . computeUnboxedP $
                               generate_v (mA_dim - k - 1) (\i -> elem_m (i + k + 1) k mA)
                    mA20    =  delay . computeUnboxedP $
                               subMatrix_m (k + 1) 0 (mA_dim - k - 1) k mAL
                    vA21'   =  delay . computeUnboxedP .
                               deepSeqArray mA20 .
                               deepSeqArray mA21 $
                               call_gemv (NoTrans mA20) vA10 (-1) 1 mA21

                    vAx1  = delay . computeUnboxedP .
                            deepSeqArray vA11' .
                            deepSeqArray vA21' .
                            add_zeros $ iif (k == mA_dim - 1) vA11' (concat_v [vA11', map_v (/eA11') vA21'])

                    mAL'  = delay . computeUnboxedP .
                            deepSeqArray vAx1 .
                            fromCols_vm $ toCols_vm mAL ++ [vAx1]

                    call_gemv !m !v1 !alpha !beta !v2 = getResultDataV (gemv gemv_ctx m v1 alpha beta v2 :: ResV gemvs RepaVector e)

                    add_zeros !v  = concat_v [generate_v k (const 0) , v]
                    {-# INLINE mA_dim #-}
                    mA_dim       = cantCols_m mA

computeUnboxedP :: (Unbox e, Shape sh) => Array D sh e -> Array U sh e
computeUnboxedP = suspendedComputeP
