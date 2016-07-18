{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module FPNLA.Operations.BLAS.Strategies.GEMV (

) where

import           FPNLA.Matrix                (asColumn_vm, toCols_vm)
import           FPNLA.Operations.BLAS       (Elt(), GEMM (gemm), GEMV (gemv))
import           FPNLA.Operations.Parameters (ResM, TransType (..), blasResultV,
                                              getResultDataM)

instance (Elt e, GEMM s m v e) => GEMV s m v e where
    gemv strat tmA vB alpha beta vC =
        blasResultV . head . toCols_vm. getResultDataM $
            call_gemm tmA pmB alpha beta pmC
        where pmB = NoTrans $ asColumn_vm vB
              pmC = asColumn_vm vC
              call_gemm mA mB alpha beta mC = gemm strat mA mB alpha beta mC :: ResM s v m e
