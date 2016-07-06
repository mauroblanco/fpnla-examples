{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

module FPNLA.Matrix.Instances.HMatrix () where

import           Control.DeepSeq             (NFData (rnf))
import           Control.Parallel.Strategies (rseq, withStrategy)
import qualified Data.Packed.Matrix          as HM (Element, Matrix,
                                                    buildMatrix, cols,
                                                    fromBlocks, fromColumns,
                                                    mapMatrix, rows, subMatrix,
                                                    toBlocksEvery, toColumns,
                                                    toRows, trans, (><), (@@>))
import qualified Data.Packed.Vector          as HV (Vector, buildVector, dim,
                                                    foldVector, fromList, join,
                                                    mapVector, zipVectorWith,
                                                    (@>))
import           FPNLA.Matrix                (Matrix (generate_m, fromList_m, transpose_m, dim_m, elem_m, subMatrix_m, fromBlocks_m, toBlocks_m, map_m), MatrixVector (fromCols_vm, toCols_vm, row_vm, col_vm), Vector (generate_v, fromList_v, concat_v, elem_v, length_v, foldr_v, map_v, zipWith_v))
{-
instance (NFData e) => NFData (HV.Vector e) where
    -- Asumo que Vector es estricto
    -- http://haskell.1045720.n5.nabble.com/NFData-instance-for-Numeric-LinearAlgebra-Matrix-td4265725.html
    rnf v = (withStrategy rseq v) `seq` ()

instance (NFData e) => NFData (HM.Matrix e) where
    -- Matrix es estricto
    -- http://haskell.1045720.n5.nabble.com/NFData-instance-for-Numeric-LinearAlgebra-Matrix-td4265725.html
    rnf m = withStrategy rseq m `seq` ()
-}
instance (HM.Element e) => Vector HV.Vector e where
    generate_v = HV.buildVector
    fromList_v = HV.fromList
    concat_v = HV.join
    elem_v pos v = v HV.@> pos
    length_v = HV.dim
    foldr_v = HV.foldVector
    map_v = HV.mapVector
    zipWith_v = HV.zipVectorWith

instance (HM.Element e) => Matrix HM.Matrix e where
    generate_m rows cols gen = HM.buildMatrix rows cols (uncurry gen)
    fromList_m = (HM.><)
    transpose_m = HM.trans
    dim_m m = (HM.rows m, HM.cols m)
    elem_m i j m = m HM.@@> (i, j)
    map_m = HM.mapMatrix
    --zipWith_m
    subMatrix_m posI posJ cantRows cantCols = HM.subMatrix (posI, posJ) (cantRows, cantCols)
    fromBlocks_m = HM.fromBlocks
    toBlocks_m = HM.toBlocksEvery

instance (HM.Element e) => MatrixVector HM.Matrix HV.Vector e where
    row_vm pos m = HM.toRows m !! pos
    col_vm pos m = HM.toColumns m !! pos
    fromCols_vm = HM.fromColumns
    toCols_vm = HM.toColumns
