{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

module FPNLA.Matrix.Instances.ArrayMatrix (
    ArrayMatrix(ArrayMatrix),
) where

import           Control.DeepSeq (NFData (rnf))
import           Data.Array      (Array, bounds, elems, listArray, (!))
import qualified Data.Foldable   (foldr)
import           FPNLA.Matrix    (Matrix (fromList_m, dim_m, elem_m, generate_m), Vector (fromList_v, elem_v, generate_v, concat_v, foldr_v, length_v))
import           FPNLA.Utils     (mapPair)

-- Las instancias de NFData para Array estan definidas en Control.DeepSeq
instance (NFData arr) => NFData (ArrayMatrix arr) where
   rnf (ArrayMatrix arr) = rnf arr


instance Vector (Array Int) e where
    -- Los indices siempre comienzan en 0!
    generate_v n f = listArray (0, n-1) [f x | x <- [0..]]
    fromList_v l = listArray (0, length l -1) l
    concat_v arrs = fromList_v $ concatMap elems arrs
    elem_v = flip (!)
    length_v = (+1) . snd . bounds
    foldr_v = Data.Foldable.foldr


instance Show e => Matrix (Array (Int,Int)) e where
    generate_m rs cs gen = fromList_m rs cs [gen i j | i <- [0..rs-1], j <- [0..cs-1]]
    fromList_m rs cs = listArray ((0,0), (rs-1, cs-1))
    dim_m = mapPair (+1) (+1) . snd . bounds
    elem_m i j = flip (!) (i,j)


newtype ArrayMatrix e = ArrayMatrix (Array (Int,Int) e) deriving (Show)
wrapIn :: Array (Int,Int) e -> ArrayMatrix e
wrapIn = ArrayMatrix
wrapOut :: ArrayMatrix e -> Array (Int,Int) e
wrapOut (ArrayMatrix arr) = arr

instance Show e => Matrix (ArrayMatrix) e where
    elem_m i j = flip (!) (i,j) . wrapOut
    dim_m = mapPair (+1) (+1) . snd . bounds . wrapOut
    fromList_m rs cs ls = wrapIn $ listArray ((0,0), (rs-1, cs-1)) ls
    generate_m rs cs gen = wrapIn $ fromList_m rs cs [gen i j | i <- [0..rs-1], j <- [0..cs-1]]

