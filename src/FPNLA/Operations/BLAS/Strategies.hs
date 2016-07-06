module FPNLA.Operations.BLAS.Strategies (
    module FPNLA.Operations.BLAS.Strategies.DataTypes
) where

import FPNLA.Operations.BLAS.Strategies.DataTypes

-- Solo importan las instancias
import FPNLA.Operations.BLAS.Strategies.GEMM.DefSeq ()
import FPNLA.Operations.BLAS.Strategies.GEMM.CBindSeq ()
import FPNLA.Operations.BLAS.Strategies.GEMM.StrassenSeq ()
import FPNLA.Operations.BLAS.Strategies.GEMM.BlocksSeq ()

import FPNLA.Operations.BLAS.Strategies.GEMM.MonadPar.DefPar ()
import FPNLA.Operations.BLAS.Strategies.GEMM.MonadPar.StrassenPar ()
import FPNLA.Operations.BLAS.Strategies.GEMM.MonadPar.BlocksPar ()

import FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.DefPar ()
import FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.StrassenPar ()
import FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.BlocksPar ()

import FPNLA.Operations.BLAS.Strategies.GEMM.Accelerate.DefPar ()

import FPNLA.Operations.BLAS.Strategies.GEMV ()

import FPNLA.Operations.BLAS.Strategies.DOT.DefSeq ()

import FPNLA.Operations.BLAS.Strategies.SYRK.DefSeq ()
import FPNLA.Operations.BLAS.Strategies.SYRK.Strategies.DefPar ()
import FPNLA.Operations.BLAS.Strategies.SYRK.MonadPar.DefPar ()


import FPNLA.Operations.BLAS.Strategies.TRSM.DefSeq ()
import FPNLA.Operations.BLAS.Strategies.TRSM.BlocksByCols ()
import FPNLA.Operations.BLAS.Strategies.TRSM.BlocksByRows ()
import FPNLA.Operations.BLAS.Strategies.TRSM.CBindSeq ()

import FPNLA.Operations.BLAS.Strategies.TRSM.MonadPar.ColsPar ()

import FPNLA.Operations.BLAS.Strategies.TRSM.Strategies.ColsPar ()