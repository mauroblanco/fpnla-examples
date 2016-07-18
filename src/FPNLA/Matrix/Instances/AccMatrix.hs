{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module FPNLA.Matrix.Instances.AccMatrix (
    AccMatrix(..),
    AccVector(..),
) where

import           FPNLA.Matrix                      (Matrix (generate_m, fromList_m, transpose_m, dim_m, elem_m, map_m, zipWith_m, subMatrix_m, fromBlocks_m, toBlocks_m), MatrixVector (row_vm, col_vm, fromCols_vm, toCols_vm), Vector (generate_v, fromList_v, concat_v, elem_v, length_v, foldr_v, map_v, zipWith_v))

import           Data.Array.Accelerate             as A ((:.) (..), Acc (),
                                                         Array (), DIM1 (),
                                                         DIM2 (), Elt (),
                                                         IsNum (), Z (Z),
                                                         arrayShape, foldAll,
                                                         fromList, indexArray,
                                                         use)
import           Data.Array.Accelerate.Interpreter as R (run)

import           Control.DeepSeq                   (NFData (rnf))

import           Debug.Trace                       (trace)

newtype AccVector e = AccVector (Acc (Array DIM1 e))
newtype AccMatrix e = AccMatrix (Acc (Array DIM2 e))

instance (NFData e, Elt e, IsNum e) => (NFData (AccMatrix e)) where
    rnf (AccMatrix m) = rnf $ seq (R.run $ foldAll (\_ _ -> 0) 0 m) ()

instance (NFData e, Elt e, IsNum e) => (NFData (AccVector e)) where
    rnf (AccVector v) = rnf $ seq (R.run $ foldAll (\_ _ -> 0) 0 v) ()

instance (Elt e) => Show (AccMatrix e) where
    show (AccMatrix m) = show m

instance (Elt e) => Show (AccVector e) where
    show (AccVector m) = show m

instance (Elt e) => Vector AccVector e where
    generate_v _ _ = trace "generate_v" undefined
    fromList_v l = AccVector . use $ fromList (Z:.(length l)) l
    concat_v _ = trace "concat_v" undefined
    elem_v _ _ = trace "elem_v" undefined
    length_v _ = trace "length_v" undefined
    foldr_v _ _ _ = trace "foldr_v" undefined
    map_v = trace "map_v" undefined
    zipWith_v = trace "zipWith_v" undefined

instance (Elt e) => Matrix AccMatrix e where
    generate_m _ _ _ = trace "generate_m" undefined
    fromList_m m n l = AccMatrix . use $ fromList (Z:.m:.n) l
    transpose_m = trace "transpose_m" undefined
    dim_m (AccMatrix m) = (rows, cols)
        where (Z :. rows :. cols) = arrayShape (R.run m)
    elem_m i j (AccMatrix m) = A.indexArray (R.run m) (Z:.i:.j)
    map_m = trace "map_m" undefined
    zipWith_m = trace "zipWith_m" undefined
    subMatrix_m _ _ _ _ = trace "subMatrix_m" undefined
    fromBlocks_m = trace "fromBlocks_m" undefined
    toBlocks_m = trace "toBlocks_m" undefined

instance (Elt e) => MatrixVector AccMatrix AccVector e  where
    row_vm _ _ = trace "row_vm" undefined
    col_vm _ _ = trace "col_vm" undefined
    fromCols_vm = trace "fromCols_vm" undefined
    toCols_vm = trace "toCols_vm" undefined
