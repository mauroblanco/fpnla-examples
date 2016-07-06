{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.GEMM.Strategies.BlocksPar () where

import           FPNLA.Matrix                               (fromBlocks_m,
                                                             map_m, toBlocks_m,
                                                             zipWith_m)
import           FPNLA.Operations.BLAS                      (GEMM (gemm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (BlockContext (getBlockDim),
                                                             BlocksPar_ST)
import           FPNLA.Operations.Parameters                (ResM, TransType (NoTrans),
                                                             blasResultM,
                                                             getResultDataM,
                                                             transTrans_m)

import           Control.DeepSeq                            (NFData)
import           Control.Parallel.Strategies                (parTraversable,
                                                             rdeepseq,
                                                             withStrategy)

instance (NFData (m e), GEMM s m v e) => GEMM (BlocksPar_ST s) m v e where
    gemm (bctx, sctx) tmA tmB alpha beta mC = blasResultM . fromBlocks_m $ generatePar rA cB (\i j -> add_m (matMultIJ i j (bmC !! i !! j)) (map_m (beta *) (bmC !! i !! j)))
        where
            (r, c) = getBlockDim bctx
            add_m = zipWith_m (+)
            generatePar m n f = withStrategy (parTraversable rdeepseq) $ generateBlocks m n f
            generateBlocks m n f = [[f i j | j <- [0 .. (n - 1)]] | i <- [0 .. (m - 1)]]
            matMultIJ i j bmCIJ = foldr1 add_m  [callGemm (bmA !! i !! k) (bmB !! k !! j) bmCIJ |  k <- [0 .. (cA - 1)]]
            callGemm mA mB mC = getResultDataM (gemm sctx (NoTrans mA) (NoTrans mB) alpha 0 mC :: ResM s v m e)
            bmA = toBlocks_m r c $ transTrans_m tmA
            bmB = toBlocks_m r c $ transTrans_m tmB
            bmC = toBlocks_m r c mC
            rA = length bmA
            cA = length (head bmA)
            cB = length (head bmB)
