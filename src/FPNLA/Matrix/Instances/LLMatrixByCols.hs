{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module FPNLA.Matrix.Instances.LLMatrixByCols (
    LVector(LVector),
    LLMatrix(LLMatrix),
) where

import           Control.DeepSeq             (NFData (rnf))
import           Control.Parallel.Strategies (rdeepseq, withStrategy)
import           Data.Foldable               (foldr')
import           Data.List                   (transpose)
import           FPNLA.Matrix                (Matrix (generate_m, transpose_m, dim_m, elem_m, map_m, zipWith_m, subMatrix_m, fromBlocks_m), MatrixVector (fromCols_vm, toCols_vm, row_vm, col_vm), Vector (generate_v, fromList_v, concat_v, elem_v, length_v, foldr_v, map_v, zipWith_v),
                                              cantCols_m)

data LVector e = LVector [e]
data LLMatrix e = LLMatrix [[e]]

getLVectorInner :: LVector t -> [t]
getLVectorInner (LVector l) = l
getLLMatrixInner :: LLMatrix t -> [[t]]
getLLMatrixInner (LLMatrix ll) = ll
newLVector :: [e] -> LVector e
newLVector = LVector
newLLMatrix :: [[e]] -> LLMatrix e
newLLMatrix = LLMatrix

instance Show e => Show (LLMatrix e) where
    show = show . getLLMatrixInner . transpose_m

instance (NFData e) => NFData (LVector e) where
    rnf v =
        withStrategy rdeepseq (getLVectorInner v) `seq` ()

instance (NFData e) => NFData (LLMatrix e) where
    rnf m =
        withStrategy rdeepseq (getLLMatrixInner m) `seq` ()

instance Vector LVector e where
    generate_v size gen = newLVector [gen x | x <- [0..(size - 1)]]
    fromList_v = newLVector
    concat_v = newLVector . concatMap getLVectorInner
    elem_v pos v = getLVectorInner v !! pos
    length_v = length . getLVectorInner
    foldr_v cons zero = foldr' cons zero . getLVectorInner
    map_v f = newLVector . map f . getLVectorInner
    zipWith_v f v1 v2 = newLVector $ zipWith f (getLVectorInner v1) (getLVectorInner v2)

instance Matrix LLMatrix e where
    generate_m rows cols gen = newLLMatrix [[gen r c | r <- [0..(rows - 1)]] | c <- [0..(cols - 1)]]
    --fromList_m m n l = se puede mejorar
    transpose_m = newLLMatrix . transpose . getLLMatrixInner
    dim_m m = (length $ head (getLLMatrixInner m), length $ getLLMatrixInner m)
    elem_m i j m = getLLMatrixInner m !! j !! i
    map_m f m = newLLMatrix . map (map f) $ getLLMatrixInner m
    zipWith_m f m1 m2 = newLLMatrix $ zipWith (zipWith f) (getLLMatrixInner m1) (getLLMatrixInner m2)
    subMatrix_m posI posJ cantRows cantCols = newLLMatrix . map (take cantRows . drop posI) . take cantCols . drop posJ . getLLMatrixInner
    fromBlocks_m = expandVert . map expandHoriz
        where
            expandVert lm = newLLMatrix [concatMap (getLVectorInner . col_vm c) lm | c <- [0 .. (cantCols_m (head lm) - 1)]]
            expandHoriz = newLLMatrix . concatMap getLLMatrixInner
    --toBlocks_m m n m1 =

instance MatrixVector LLMatrix LVector e where
    row_vm pos m = newLVector $ map (!! pos) (getLLMatrixInner m)
    col_vm pos m = newLVector $ getLLMatrixInner m !! pos
    fromCols_vm = newLLMatrix . map getLVectorInner
    toCols_vm m = map newLVector $ getLLMatrixInner m
