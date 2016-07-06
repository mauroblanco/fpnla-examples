{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.SYRK.DefSeq () where


import           FPNLA.Matrix                               (MatrixVector,
                                                             foldr_v,
                                                             generate_m,
                                                             generate_v)
import           FPNLA.Operations.BLAS                      (SYRK (syrk))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefSeq)
import           FPNLA.Operations.Parameters                (Elt,
                                                             TransType (..),
                                                             blasResultM,
                                                             dimTrans_m,
                                                             dimTriang,
                                                             elemSymm,
                                                             elemTrans_m)

instance  (Elt e, MatrixVector m v e) => SYRK DefSeq m v e where
    syrk _ alpha pmA beta pmB
        | p /= p' = error "syrk: incompatible ranges"
        | otherwise = blasResultM $ generate_m p p (\i j -> (alpha * pmAMultIJ i j) + beta * elemSymm i j pmB)
        where
            (p, p') = dimTriang pmB
            matMultIJ i j tmA tmB = foldr_v (+) 0 (generate_v (snd $ dimTrans_m tmA) (\k -> (*) (elemTrans_m i k tmA) (elemTrans_m k j tmB)) :: v e)
            pmAMultIJ i j =
                case pmA of
                    (NoTrans mA) -> matMultIJ i j pmA (Trans mA)
                    (Trans mA) -> matMultIJ i j (Trans mA) pmA
                    (ConjTrans mA) -> matMultIJ i j (Trans mA) pmA
