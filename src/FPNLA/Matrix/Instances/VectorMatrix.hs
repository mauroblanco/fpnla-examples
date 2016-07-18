{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module FPNLA.Matrix.Instances.VectorMatrix (
    VVector,
    VMatrix(VMatrix),
) where

import           Control.DeepSeq             (NFData, rnf)
import           Control.Parallel.Strategies (rdeepseq, withStrategy)
import qualified Data.Foldable               (foldr)
import qualified Data.Vector                 as DV (Vector, concat, drop,
                                                    fromList, generate, head,
                                                    length, map, take, toList,
                                                    zipWith, (!), (++))
import           FPNLA.Matrix                (Matrix (..), MatrixVector (..),
                                              Vector (..), cantCols_m)
import           FPNLA.Utils                 (iif)
{-
instance (NFData a) => NFData (DV.Vector a) where
  rnf v = evalVect (DV.length v)
    where evalVect i
            | i > 0 = withStrategy rdeepseq (v DV.! (i-1)) `seq` evalVect (i-1)
            | otherwise = ()
-}
instance (NFData a) => NFData (VMatrix a) where
    rnf (VMatrix v) = withStrategy rdeepseq v `seq` ()

--
type VVector = DV.Vector
instance Vector (DV.Vector) e where
    generate_v = DV.generate
    fromList_v = DV.fromList
    concat_v = DV.concat
    elem_v = flip (DV.!)
    length_v = DV.length
    foldr_v = Data.Foldable.foldr
    map_v = DV.map
    zipWith_v = DV.zipWith

--
newtype VMatrix e = VMatrix (DV.Vector (DV.Vector e)) deriving (Show)
wrapIn :: DV.Vector (DV.Vector e) -> VMatrix e
wrapIn = VMatrix
wrapOut :: VMatrix e -> DV.Vector (DV.Vector e)
wrapOut (VMatrix vv) = vv

instance Matrix VMatrix e where
    generate_m cr cc gen = wrapIn $ DV.generate cc (\j -> DV.generate cr (`gen` j))
    --fromList_m =
    --transpose_m =
    -- Se guarda por columnas!
    dim_m m = (iif (cc > 0) cr 0, cc)
        where cc = DV.length (wrapOut m)
              cr = DV.length $ DV.head (wrapOut m)
    elem_m i j m = wrapOut m DV.! j DV.! i
    map_m f m = wrapIn $ DV.map (DV.map f) (wrapOut m)
    zipWith_m f m1 m2 = wrapIn $ DV.zipWith (DV.zipWith f) (wrapOut m1) (wrapOut m2)
    subMatrix_m posI posJ cantRows cantCols =
        wrapIn . DV.map (DV.take cantRows . DV.drop posI) . DV.take cantCols . DV.drop posJ . wrapOut
    fromBlocks_m = expandVert . map expandHoriz
        where
            expandVert lm = wrapIn . DV.generate (cantCols_m (head lm)) $ (\c -> foldr1 (DV.++) $ map (col_vm c) lm)
            expandHoriz = wrapIn . foldr1 (DV.++) . map wrapOut
    --toBlocks_m

instance MatrixVector VMatrix DV.Vector e where
    row_vm i m = DV.map (DV.! i) (wrapOut m)
    col_vm j m = wrapOut m DV.! j
    fromCols_vm = wrapIn . DV.fromList
    toCols_vm = DV.toList . wrapOut
