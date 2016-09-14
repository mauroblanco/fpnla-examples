
module FPNLA.Operations.BLAS_M.Strategies.GEMM.Utils (
    asTrans,
    asNoTrans
) where


import           FPNLA.Matrix_M                             (Matrix(dim_m, elem_m, generate_m))
import           FPNLA.Operations.Parameters                (Elt(..), TransType(..))


-- ConjTrans could be optimized
asTrans :: (Elt e, Matrix mon m e) => TransType (m e) -> mon (m e)
asTrans mt =
    case mt of
        NoTrans m -> apply m (\i j -> elem_m j i m)
        Trans m -> return m
        ConjTrans m -> apply m (\i j -> getConjugate <$> elem_m i j m)
    where
        apply m f =
            do
                (r, c) <- dim_m m
                generate_m r c f

asNoTrans :: (Elt e, Matrix mon m e) => TransType (m e) -> mon (m e)
asNoTrans mt =
    case mt of
        NoTrans m -> return m
        Trans m -> apply m (\i j -> elem_m j i m)
        ConjTrans m -> apply m (\i j -> getConjugate <$> elem_m j i m)
    where
        apply m f =
            do
                (r, c) <- dim_m m
                generate_m r c f

