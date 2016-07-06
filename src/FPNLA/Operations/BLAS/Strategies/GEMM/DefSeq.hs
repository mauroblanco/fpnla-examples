{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.DefSeq () where

import           FPNLA.Matrix                               (MatrixVector,
                                                             elem_m, foldr_v,
                                                             generate_m,
                                                             generate_v)
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefSeq)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             dimTrans_m,
                                                             elemTrans_m)

instance  (Elt e, MatrixVector m v e) => GEMM DefSeq m v e where
    gemm _ pmA pmB alpha beta mC
        | p /= p' = error "gemm: incompatible ranges"
        | otherwise = blasResultM . generate_m m n $ (\i j -> (alpha * matMultIJ i j) + beta * elem_m i j mC)
        where
            (m, p) = dimTrans_m pmA
            (p',n) = dimTrans_m pmB
            matMultIJ i j = foldr_v (+) 0 (generate_v p (\k -> (*) (elemTrans_m i k pmA) (elemTrans_m k j pmB)) :: v e)
