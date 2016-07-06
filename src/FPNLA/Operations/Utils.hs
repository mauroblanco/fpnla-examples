
module FPNLA.Operations.Utils (
    matrixToPtr,
    ptrToMatrix,
    transToForeign,
    triangToForeign,
    unitToForeign,
    unsafePerformIO,
) where

import FPNLA.Matrix (Matrix(dim_m, elem_m, generate_m))
import FPNLA.Operations.Parameters (UnitType(NoUnit, Unit), TransType(NoTrans, Trans, ConjTrans), TriangType(Upper, Lower), Elt)

import Foreign.Storable (Storable, peekElemOff)
import Foreign.Marshal.Array (newArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Foreign.BLAS as L3 (Trans(ConjTrans, NoTrans, Trans), Uplo(Lower, Upper), Diag(Unit, NonUnit))

-- ----------------------------------------------------------------------------
-- CBind:
-- ----------------------------------------------------------------------------
matrixToPtr :: (Storable e, Elt e, Matrix m e) => m e -> IO (Ptr e)
matrixToPtr m = do
    let (rs, cs) = dim_m m
    newArray [elem_m i j m | j <- [0..cs-1], i <- [0..rs-1]]

ptrToMatrix :: (Storable e, Elt e, Matrix m e) => Int -> Int -> Ptr e -> IO (m e)
ptrToMatrix m n ptr = return $ generate_m m n (\i j -> unsafePerformIO $ peekElemOff ptr $ j*m+i)

transToForeign :: TransType t -> L3.Trans
transToForeign (NoTrans _)    = L3.NoTrans
transToForeign (Trans _)      = L3.Trans
transToForeign (ConjTrans _)  = L3.ConjTrans

triangToForeign :: TriangType t -> L3.Uplo
triangToForeign (Lower _) = L3.Lower
triangToForeign (Upper _) = L3.Upper

unitToForeign :: UnitType t -> L3.Diag
unitToForeign (Unit _) = L3.Unit
unitToForeign (NoUnit _) = L3.NonUnit
