{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.DefPar () where

import           Control.DeepSeq                            (NFData)
import           Control.Parallel.Strategies                (parMap, rdeepseq)
import           FPNLA.Matrix                               (MatrixVector (fromCols_vm),
                                                             elem_m, foldr_v,
                                                             generate_v)
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefPar_ST)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             dimTrans_m,
                                                             elemTrans_m)

instance (NFData (v e), Elt e, MatrixVector m v e) => GEMM DefPar_ST m v e where
    gemm _ pmA pmB alpha beta mC
        | p /= p' = error "gemm: incompatible ranges"
        | otherwise = blasResultM $ generatePar_m m n (\i j -> (alpha * matMultIJ i j) + beta * elem_m i j mC)
        where
            (m, p) = dimTrans_m pmA
            (p',n) = dimTrans_m pmB
            matMultIJ i j = foldr_v (+) 0 (generate_v p (\k -> (*) (elemTrans_m i k pmA) (elemTrans_m k j pmB)) :: v e)
            generatePar_m m n gen = fromCols_vm . parMap rdeepseq (\j -> generate_v m (`gen` j) :: v e) $ [0 .. (n - 1)]

