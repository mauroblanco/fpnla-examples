{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.Accelerate.DefPar () where

import           FPNLA.Matrix.Instances.AccMatrix           (AccMatrix (..),
                                                             AccVector (..))
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefPar_Acc)
import           FPNLA.Operations.Parameters                (Elt,
                                                             TransType (..),
                                                             blasResultM)

import           Data.Array.Accelerate                      ((:.) (..), Acc,
                                                             All (All), Array,
                                                             DIM2, Exp, IsNum,
                                                             Z (Z),
                                                             constant,
                                                             shape)
import qualified Data.Array.Accelerate                      as A (Elt, fold,
                                                                  lift, map,
                                                                  replicate,
                                                                  transpose,
                                                                  unlift,
                                                                  zipWith)

instance (Elt e, A.Elt e, IsNum e) => GEMM DefPar_Acc AccMatrix AccVector e where
    gemm _ pmA pmB alpha beta (AccMatrix mC) =
        blasResultM (AccMatrix mC')

        where
            alpha_expr = constant alpha
            beta_expr = constant beta
            mC' = A.map (beta_expr*) . A.zipWith (+) mC $ matMul mA mB
            mA = A.map (alpha_expr*) $ unAccTrans pmA
            mB = unAccTrans pmB
            unAccTrans pm = case pm of (NoTrans (AccMatrix m)) -> m
                                       (Trans (AccMatrix m)) -> A.transpose m
                                       (ConjTrans (AccMatrix m)) -> A.transpose m --no hay complejos en A.Elt


matMul :: (A.Elt e, IsNum e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e)
matMul arr brr
  = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
  where
    Z :. rowsA :. _     = A.unlift (shape arr) :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = A.unlift (shape brr) :: Z :. Exp Int :. Exp Int

    arrRepl             = A.replicate (A.lift $ Z :. All   :. colsB :. All) arr
    brrRepl             = A.replicate (A.lift $ Z :. rowsA :. All   :. All) (A.transpose brr)
{-
matMul2 :: (A.Elt e, IsNum e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e)
matMul2 arr brr =
    let
        (Z:.rowsA:.colsA) = A.unlift (shape arr) :: Z :. Exp Int :. Exp Int
        (Z:.rowsB:.colsB) = A.unlift (shape brr) :: Z :. Exp Int :. Exp Int
        -- Transpongo mB:
        mB'_acc = backpermute (A.lift $ Z:.colsB:.rowsB) (\e -> A.uncurry index2 $ A.lift (A.snd $ unindex2 e, A.fst $ unindex2 e)) brr
        repB = A.replicate (A.lift $ Z:.rowsA:.All:.All) mB'_acc
        repA = A.replicate (A.lift $ Z:.All:.colsB:.All) arr
    in A.fold1 (+) $ A.zipWith (*) repA repB
-}