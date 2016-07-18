{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}

module FPNLA.Matrix.Instances.HMatrix (buildVector, buildMatrix) where

import           Control.DeepSeq               (NFData (rnf))
import           Control.Parallel.Strategies   (rseq, withStrategy)
import "hmatrix" Numeric.LinearAlgebra hiding  (Matrix, Vector)
--import           Numeric.LinearAlgebra.HMatrix
--import "hmatrix" Numeric.LinearAlgebra         (buildVector)
import           Numeric.LinearAlgebra.HMatrix hiding (Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix  as HM (Matrix, Vector)
import           FPNLA.Matrix                  (Matrix (generate_m, fromList_m, transpose_m, dim_m, elem_m, subMatrix_m, fromBlocks_m, toBlocks_m, map_m), MatrixVector (fromCols_vm, toCols_vm, row_vm, col_vm), Vector (generate_v, fromList_v, concat_v, elem_v, length_v, foldr_v, map_v, zipWith_v))
{-
instance (NFData e) => NFData (Vector e) where
    -- Asumo que Vector es estricto
    -- http://haskell.1045720.n5.nabble.com/NFData-instance-for-Numeric-LinearAlgebra-Matrix-td4265725.html
    rnf v = (withStrategy rseq v) `seq` ()

instance (NFData e) => NFData (Matrix e) where
    -- Matrix es estricto
    -- http://haskell.1045720.n5.nabble.com/NFData-instance-for-Numeric-LinearAlgebra-Matrix-td4265725.html
    rnf m = withStrategy rseq m `seq` ()
-}

buildVector :: Int -> (Int -> e) -> HM.Vector e
buildVector size f = undefined

buildMatrix :: Int -> Int -> (Int -> Int -> e) -> HM.Matrix e
buildMatrix rows cols f = undefined

instance (Element e) => Vector HM.Vector e where
    generate_v = buildVector
    fromList_v = fromList
    concat_v = undefined -- joinsas
    elem_v pos v = undefined -- v @> pos
    length_v = undefined -- dim
    foldr_v = undefined -- foldVector
    map_v = undefined -- mapVector
    zipWith_v = undefined -- zipVectorWith

instance (Element e) => Matrix HM.Matrix e where
    generate_m = buildMatrix
    fromList_m = (><)
    transpose_m = undefined -- trans
    dim_m m = (rows m, cols m)
    elem_m i j m = undefined -- m @@> (i, j)
    map_m = undefined -- mapMatrix
    --zipWith_m
    subMatrix_m posI posJ cantRows cantCols = subMatrix (posI, posJ) (cantRows, cantCols)
    fromBlocks_m = fromBlocks
    toBlocks_m = toBlocksEvery

instance (Element e) => MatrixVector HM.Matrix HM.Vector e where
    row_vm pos m = toRows m !! pos
    col_vm pos m = toColumns m !! pos
    fromCols_vm = fromColumns
    toCols_vm = toColumns
