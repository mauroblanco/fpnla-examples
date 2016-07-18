{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FPNLA.Matrix.Instances.RepaMatrix (RepaMatrix, RepaVector) where

import           Control.DeepSeq              (NFData (rnf))
import           FPNLA.Matrix                 (Matrix (generate_m, fromList_m, transpose_m, dim_m, elem_m, map_m, zipWith_m, subMatrix_m, fromBlocks_m), MatrixVector (row_vm, col_vm, fromCols_vm), Vector (generate_v, fromList_v, concat_v, elem_v, length_v, foldr_v, map_v, zipWith_v))

import           Data.Array.Repa              ((:.) (..), All (..), Any (..),
                                               Array, D, DIM1, DIM2, Shape,
                                               Source, Z (..), append,
                                               computeUnboxedS, deepSeqArray,
                                               delay, extend, extent, extract,
                                               fromFunction, fromFunction,
                                               fromListUnboxed, index, map,
                                               size, slice, toList, transpose,
                                               zipWith)
import           Data.Array.Repa.Eval         ()
import           Data.Array.Repa.Repr.Unboxed (Unbox ())
import           Data.Foldable                (foldr')
import           Prelude                      (Int, Show (..), foldr1, length,
                                               ($), (.))
import qualified Prelude                      as P hiding (Show)

type RepaVector = Array D DIM1
type RepaMatrix = Array D DIM2

instance (Shape sh, Show sh, Show e, Unbox e) => Show (Array D sh e) where
    show = show . computeUnboxedS

instance (Source r e, Shape sh, NFData e) => NFData (Array r sh e) where
    rnf m = deepSeqArray m ()

instance (Unbox e) => Vector RepaVector e where
    generate_v size gen = fromFunction (Z:.size) (\(Z:.pos) -> gen pos)
    fromList_v l = delay $ fromListUnboxed (Z :. length l) l
    concat_v = foldr1 append
    elem_v pos v = index v (Z:.pos)
    length_v v = size $ extent v
    --foldr_v cons zero v = foldAllS cons zero v -- foldAllS no sirve por su tipo
    foldr_v cons zero v = foldr' cons zero (toList v) -- es lo que hay...
    map_v = map
    zipWith_v = zipWith

instance (Unbox e) => Matrix RepaMatrix e where
    generate_m rows cols gen = fromFunction (Z:.rows:.cols) (\(Z:.i:.j) -> gen i j)
    fromList_m m n l = delay $ fromListUnboxed (Z:.m:.n) l
    transpose_m = transpose
    dim_m = (\(Z:.rows:.cols) -> (rows, cols)) . extent
    elem_m i j m = index m (Z:.i:.j)
    map_m = map
    zipWith_m = zipWith
    subMatrix_m posI posJ cantRows cantCols = extract (Z:.posI:.posJ) (Z:.cantRows:.cantCols)
    fromBlocks_m = transpose . foldr1 append . P.map (transpose . foldr1 append)
    --toBlocks_m = -- Queda el default usado submatrices (extract)

instance (Unbox e) => MatrixVector RepaMatrix RepaVector e  where
    row_vm pos m = slice m (Any:.pos:.All)
    col_vm pos m = slice m (Any:.pos)
    fromCols_vm = foldr1 append . P.map (transpose . extend (Any:.(1::Int):.All))
    --toCols_vm = -- Queda el default usado submatrices (extract)
