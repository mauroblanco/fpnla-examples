{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PackageImports        #-}

module FPNLA.Operations.LAPACK.Strategies.POTRF.HMatrixBindSeq (

) where

import "hmatrix" Numeric.LinearAlgebra                        (Field(), chol, trustSym)
import           FPNLA.Matrix                                 (MatrixVector,
                                                               cantCols_m,
                                                               cantRows_m,
                                                               elem_m,
                                                               generate_m,
                                                               transpose_m)
import           FPNLA.Matrix.Instances.HMatrix               ()
import           FPNLA.Operations.LAPACK                      (POTRF (potrf))
import           FPNLA.Operations.LAPACK.Strategies.DataTypes (HMatrixBindSeq)
import           FPNLA.Operations.Parameters                  (Elt, blasResultM,
                                                               unTriangT)

instance (Field e, Elt e, MatrixVector m v e) => POTRF HMatrixBindSeq m v e where
    potrf _ pmA = blasResultM . fromHMatrix . chol . trustSym . toHMatrix $ mA
        where mA = snd $ unTriangT pmA
              fromHMatrix m = transpose_m $ generate_m (cantRows_m m) (cantCols_m m) (\i j -> elem_m i j m)
              toHMatrix m = generate_m (cantRows_m m) (cantCols_m m) (\i j-> elem_m i j m)

