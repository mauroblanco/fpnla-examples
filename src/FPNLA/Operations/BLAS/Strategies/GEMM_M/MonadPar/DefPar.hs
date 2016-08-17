{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM_M.MonadPar.DefPar () where

import           Control.DeepSeq                            (NFData)
import           Control.Monad.Par                          as MP (Par, parMap)
import           FPNLA.Matrix_M
import           FPNLA.Operations.BLAS_M
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefPar_MP)
import           FPNLA.Operations.Parameters                (Elt, blasResultM,
                                                             TransType(..))

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
--import Debug.Trace (trace)

instance (Elt e, Matrix IO m e, Vector IO v e, RowMatrixVector IO m v e) => GEMM_M IO DefPar_MP m v e where
    gemm_m _ (NoTrans mA) (Trans mB) alpha beta mC =
        do
            (_, _) <- dim_m mA
            (_, p) <- dim_m mB
            let matMultIJ i j =
                    foldl (\mr x ->
                        do
                            r <- mr
                            maIX <- elem_m i x mA
                            mbXJ <- elem_m j x mB
                            return $ r + maIX * mbXJ) (return 0) [0 .. p - 1]
--            rows :: [v e] <- toRows_vm mC
--            mvars <- traverse (\(r, i) -> do
--                done <- newEmptyMVar
--                _ <- forkIO (do
--                    v' <- update_v (\j ->
--                        do
--                            multIJ <- matMultIJ i j
--                            mcIJ <- elem_m i j mC
--                            return $ alpha * multIJ + beta * mcIJ) r
--                    _ <- evaluate v'
--                    putMVar done ()
--                    return ())
--                return done) $ zip rows [0 .. ]
--            _ <- traverse takeMVar mvars
--            return $ blasResultM mC
            blasResultM <$> update_m (\i j ->
                do
                    multIJ <- matMultIJ i j
                    mcIJ <- elem_m i j mC
                    return $ alpha * multIJ + beta * mcIJ) mC
    gemm_m _ _ _ _ _ _ = error "TODO!"
