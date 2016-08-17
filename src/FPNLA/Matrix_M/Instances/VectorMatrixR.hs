{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}

module FPNLA.Matrix_M.Instances.VectorMatrixR (

    VMatrixR(),
    DVM.MVector(),
    showVMatrixR,
    showVector

) where

import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import           FPNLA.Matrix_M              (Matrix (..), RowMatrixVector (..),
                                             Vector (..))
import           FPNLA.Utils                 (splitSlice)

import           Control.Monad.Primitive

data VMatrixR s e = VMatrixR (DV.Vector (DVM.MVector s e))

showVMatrixR :: Show e => VMatrixR RealWorld e -> IO String
showVMatrixR (VMatrixR m) =
    show <$> traverse showVector m
showVector :: Show e => DV.MVector RealWorld e -> IO String
showVector = fmap show . DV.freeze

instance Vector IO (DVM.MVector RealWorld) e where

    fromList_v l =
        do
            v <- DVM.new $ length l
            _ <- traverse (uncurry (DVM.write v)) $ zip [0 .. ] l
            return v
    elem_v = flip DVM.read
    length_v = return . DVM.length
    slice_v i j = return . DVM.slice i j
    update_v f v =
        do
            _ <- traverse (\p -> DVM.write v p =<< f p) [0 .. DVM.length v - 1]
            return v

instance Matrix IO (VMatrixR RealWorld) e where

    fromList_m _ n l = fmap (VMatrixR . DV.fromList) . traverse fromList_v $ splitSlice n l

    dim_m (VMatrixR m) = return (DV.length m, DVM.length $ m DV.! 0)

    elem_m i j (VMatrixR m) = DVM.read (m DV.! i) j

    subMatrix_m posI posJ cantRows cantCols (VMatrixR m) =
        return . VMatrixR . DV.map (DVM.slice posJ cantCols) $ DV.slice posI cantRows m

    update_m f (VMatrixR m) =
        do
            _ <- traverse (\i -> update_v (f i) $ m DV.! i) [0 .. DV.length m - 1]
            return . VMatrixR $ m

instance RowMatrixVector IO (VMatrixR RealWorld) (DVM.MVector RealWorld) e where

    row_vm pos (VMatrixR mat) = return $ mat DV.! pos
