{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.SYRK.Strategies.DefPar () where


import           Control.DeepSeq                            (NFData)
import           Control.Parallel.Strategies                (parMap, rdeepseq)
import           FPNLA.Matrix                               (MatrixVector,
                                                             foldr_v,
                                                             fromCols_vm,
                                                             generate_v)
import           FPNLA.Operations.BLAS                      (SYRK (syrk))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefPar_ST)
import           FPNLA.Operations.Parameters                (Elt,
                                                             TransType (..),
                                                             blasResultM,
                                                             dimTrans_m,
                                                             dimTriang,
                                                             elemSymm,
                                                             elemTrans_m)

instance  (NFData (v e), Elt e, MatrixVector m v e) => SYRK DefPar_ST m v e where
    syrk _ alpha pmA beta pmB
        | p /= p' = error "syrk: incompatible ranges"
        | otherwise = blasResultM $ generatePar_m p p (\i j -> (alpha * pmAMultIJ i j) + beta * elemSymm i j pmB)
        where
            (p, p') = dimTriang pmB
            matMultIJ i j tmA tmB = foldr_v (+) 0 (generate_v (snd $ dimTrans_m tmA) (\k -> (*) (elemTrans_m i k tmA) (elemTrans_m k j tmB)) :: v e)
            pmAMultIJ i j =
                case pmA of
                    (NoTrans mA) -> matMultIJ i j pmA (Trans mA)
                    (Trans mA) -> matMultIJ i j (Trans mA) pmA
                    (ConjTrans mA) -> matMultIJ i j (Trans mA) pmA
            generatePar_m m n gen = fromCols_vm . parMap rdeepseq (\j -> generate_v m (`gen` j) :: v e) $ [0 .. (n - 1)]
