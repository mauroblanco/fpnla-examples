{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.LAPACK.Strategies.POTRF.HMatrixBindSeq (

) where

import qualified Data.Packed.Matrix                           as HM
import           FPNLA.Matrix                                 (MatrixVector,
                                                               cantCols_m,
                                                               cantRows_m,
                                                               elem_m,
                                                               generate_m,
                                                               transpose_m)
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (HMatrixBindSeq)
import           FPNLA.Operations.Parameters                  (Elt, blasResultM,
                                                               unTriangT)
import           Numeric.LinearAlgebra.Algorithms

instance (Field e, Elt e, MatrixVector m v e) => POTRF HMatrixBindSeq m v e where
    potrf _ pmA = blasResultM . fromHMatrix . chol . toHMatrix $ mA
        where mA = snd $ unTriangT pmA
              fromHMatrix m = transpose_m $ generate_m (HM.rows m) (HM.cols m) (\i j -> m HM.@@> (i, j))
              toHMatrix m = HM.buildMatrix (cantRows_m m) (cantCols_m m) (\(i, j)-> elem_m i j m)

