{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE FlexibleContexts #-} --TODO sacar al terminar de debugear junto con el Show

module FPNLA.Operations.LAPACK.Strategies.POTRF.DefSeq (

) where


import           FPNLA.Matrix                                 (MatrixVector,
                                                               cantCols_m,
                                                               concat_v, elem_m,

                                                               fromCols_vm,
                                                               fromList_v,
                                                               generate_v,
                                                               map_v,
                                                               subMatrix_m,
                                                               toCols_vm)
import           FPNLA.Operations.BLAS                        (DOT (dot),
                                                               GEMV (gemv))
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (CholLLVSeq)
import           FPNLA.Operations.Parameters                  (Elt, ResV, ResS,
                                                               TransType (..),
                                                               TriangType (..),
                                                               blasResultM,
                                                               getConjugate,
                                                               getResultDataV,
                                                               getResultDataS,
                                                               unTriangT)
import           FPNLA.Utils                                  (iif)
--import Debug.Trace

{-
    Yo entiendo que la implementacion de lapack es el "The Cholesky Banachiewicz
    and Cholesky Crout algorithms", que seria lo mismo al "Left-looking variant".
-}


instance (Elt e, MatrixVector m v e, DOT dots v e, GEMV gemvs m v e) =>
    POTRF (CholLLVSeq dots gemvs) m v e where
    potrf (dot_ctx, gemv_ctx) pmA =
        case pmA of
            (Lower _) -> blasResultM $ potrf_l 0 undefined
            (Upper _) -> undefined
        where (_, mA) = unTriangT pmA
              mA_dim = cantCols_m mA
              call_gemv m v1 alpha beta v2 = getResultDataV (gemv gemv_ctx (NoTrans m) v1 alpha beta v2 :: ResV gemvs v e)
              potrf_l :: Int -> m e -> m e
              potrf_l (k@0) _  = let mAkk = sqrt $ elem_m k k mA
                                     col_mAk :: v e = generate_v (mA_dim - k - 1) (\i -> elem_m (i + k + 1) k mA)
                                     col_mLk :: v e = iif (k == mA_dim - 1) (fromList_v [mAkk]) $
                                                      concat_v [fromList_v [mAkk], map_v (/mAkk) col_mAk]
                                 in potrf_l (k + 1) $ fromCols_vm [col_mLk]
              potrf_l k mL
                  | k == mA_dim = mL
                  | otherwise = let row_mLk :: v e = generate_v k (\j -> elem_m k j mL)
                                    row_mLk_conj = map_v getConjugate row_mLk
                                    mAkk = sqrt $ elem_m k k mA - getResultDataS (dot dot_ctx row_mLk row_mLk_conj :: ResS dots e)
                                    col_mAk :: v e = generate_v (mA_dim - k - 1) (\i -> elem_m (i + k + 1) k mA)
                                    sub_mL = subMatrix_m (k + 1) 0 (mA_dim - k - 1) k mL
                                    col_mAk' = call_gemv sub_mL row_mLk (-1) 1 col_mAk
                                    col_mLk :: v e = add_zeros . iif (k == mA_dim - 1) (fromList_v [mAkk]) $
                                                     concat_v [fromList_v [mAkk], map_v (/mAkk) col_mAk']
                                    add_zeros v = concat_v [generate_v k (const 0) , v]
                                in potrf_l (k + 1) $ fromCols_vm $ toCols_vm mL ++ [col_mLk]




{-
import Lapack (Res, ResV, Elt, getConjugate, LapackPotrfOp(potrf), BlasDotOp(dot), BlasGemvOp(gemv),
              blasResult, getResultDataV, TriangType(..), unTriangT, TransType(..))
import Matrix (MatrixVector, cantCols_m, generate_v, generate_m, toCols_vm, fromCols_vm,
              subMatrix_m, elem_m, elem_v, fromList_v, concat_v, map_v)
import Lapack.Strategies.DataTypes (CholLLVSeq)
import Utils (iif)

--import Debug.Trace

--trace' s a = trace (s ++ ": " ++ (show a)) a

instance (Elt e, MatrixVector m v e, BlasDotOp v e, BlasGemvOp gemvs m v e) =>
    LapackPotrfOp (CholLLVSeq gemvs) m v e where
    potrf gemv_ctx (Lower mA)
        =  blasResult $ chol_l gemv_ctx 0 mA (generate_m 0 0 undefined)
        where
            chol_l gemv_ctx k mA mAL
                | k == 0  =  let eA11'  = sqrt $ eA11
                                 vA11'  = fromList_v [eA11']
                                 vA21'  = map_v (/eA11') mA21
                                 vAx1   = iif (k == mA_dim - 1) vA11' (concat_v [vA11', vA21'])
                                 mAL'   = fromCols_vm [vAx1]
                             in chol_l gemv_ctx (k + 1) mA mAL'
                | k == mA_dim  =  mAL
                | otherwise    =  chol_l gemv_ctx (k + 1) mA mAL'

                where
                    vA10    = generate_v k (\j -> elem_m k j mAL)
                    vA10_c  = map_v getConjugate vA10
                    eA11    = elem_m k k mA
                    eA11'   = sqrt $ eA11 - (dot vA10 vA10_c)
                    vA11'   = fromList_v [eA11']
                    mA21    = generate_v (mA_dim - k - 1) (\i -> elem_m (i + k + 1) k mA)
                    mA20    = subMatrix_m (k + 1) 0 (mA_dim - k - 1) k mAL
                    vA21'   = call_gemv (NoTrans mA20) vA10 (-1) 1 mA21

                    vAx1  = add_zeros . iif (k == mA_dim - 1) vA11' $
                                     concat_v [vA11', map_v (/eA11') vA21']

                    mAL'  = fromCols_vm $ (toCols_vm mAL) ++ [vAx1]

                    call_gemv m v1 alpha beta v2 = getResultDataV $ ((gemv gemv_ctx m v1 alpha beta v2) :: ResV gemvs v e)

                    add_zeros v  = concat_v [generate_v k (\i -> 0) , v]
                    mA_dim       = cantCols_m mA
-}
