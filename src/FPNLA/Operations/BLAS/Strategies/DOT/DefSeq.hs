{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FPNLA.Operations.BLAS.Strategies.DOT.DefSeq (

) where

import           FPNLA.Matrix                (Vector, foldr_v, zipWith_v)
import           FPNLA.Operations.BLAS       (DOT (dot))
import           FPNLA.Operations.Parameters (Elt, blasResultS)
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefSeq)

instance (Elt e, Vector v e) => DOT DefSeq v e where
    dot _ pvA pvB = blasResultS . foldr_v (+) 0 $ zipWith_v (*) pvA pvB
