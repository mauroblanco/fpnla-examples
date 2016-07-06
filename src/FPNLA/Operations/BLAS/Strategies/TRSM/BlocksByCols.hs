{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FPNLA.Operations.BLAS.Strategies.TRSM.BlocksByCols () where

import           FPNLA.Matrix                               (cantCols_m,
                                                             cantRows_m,
                                                             diagonalBlock,
                                                             fromBlocks_m,
                                                             generate_m,
                                                             subMatrix_m,
                                                             verticalBlock)
import           FPNLA.Operations.BLAS                      (GEMM (gemm),
                                                             TRSM (trsm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (BlocksByCols, SqrBlockContext (getSqrBlockDim))
import           FPNLA.Operations.Parameters                (ResM, TransType (NoTrans, Trans, ConjTrans), TriangType (Upper, Lower),
                                                             blasResultM,
                                                             getResultDataM,
                                                             transTrans_m,
                                                             unTransT,
                                                             unTriangT, unUnitT)

instance (GEMM gs m v e, TRSM ts m v e) => TRSM (BlocksByCols gs ts) m v e where
    --trsm :: StratCtx s -> e -> TransType (TriangType (UnitType (m e))) -> m e -> Res s v m e
    trsm _ 0 _ mB = blasResultM $ generate_m (cantRows_m mB) (cantCols_m mB) (\_ _ -> 0)
    trsm (ctx, gctx, tctx) alpha tratmA mB =
        blasResultM $ solveTrsmAux alpha (tritAux (transTrans_m (trat mA))) mB []
        where

            (trat, tritmA) = unTransT tratmA

            (_, utmA) = unTriangT tritmA
            (ut, mA) = unUnitT utmA

            tritAux = case (tritmA, tratmA) of
                          (Lower _, NoTrans _) -> Lower
                          (Upper _, Trans _) -> Lower
                          (Upper _, ConjTrans _) -> Lower
                          _ -> Upper

            blockDim = getSqrBlockDim ctx
            callTrsmBase trit ut alpha mA mB = getResultDataM (trsm tctx alpha (NoTrans (trit (ut mA))) mB :: ResM ts v m e)
            callGemmBase alpha mA mB beta mC = getResultDataM (gemm gctx (NoTrans mA) (NoTrans mB) alpha beta mC :: ResM gs v m e)
            solveTrsmAux alpha (tritmA@(Lower _)) mB listX =
                if fin then
                    fromBlocks_m . map (:[]) . reverse $ listX'
                else
                    solveTrsmAux alpha (trit mA) mB' listX'
                where
                    dim_mA = cantRows_m mA
                    (trit, mA) = unTriangT tritmA
                    listX' = mX_i:listX
                    i = length listX
                    fin = blockDim*(i + 1) >= dim_mA
                    tailVerticalBlock m = subMatrix_m blockDim 0 (cantRows_m m - blockDim) (cantCols_m m) m
                    (mX_i, mB')
                        | null listX =
                            let
                                mA_00 = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) 0 mA
                                mB_0 = verticalBlock (getSqrBlockDim ctx) 0 mB
                                mB_inf = tailVerticalBlock mB
                            in
                                (callTrsmBase trit ut alpha mA_00 mB_0, mB_inf)
                        | otherwise =
                            let
                                mX = head listX
                                mA_next = subMatrix_m
                                    (blockDim*i)
                                    (blockDim*(i - 1))
                                    (dim_mA - (blockDim*i))
                                    (min blockDim (dim_mA - (blockDim*(i - 1))))
                                    mA
                                mB' = callGemmBase (negate (1/alpha)) mA_next mX 1 mB
                                mB'_inf = tailVerticalBlock mB'
                                mA_ii = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) i mA
                                mB_i' = verticalBlock (getSqrBlockDim ctx) 0 mB'
                            in
                                (callTrsmBase trit ut alpha mA_ii mB_i', mB'_inf)
            solveTrsmAux alpha (tritmA@(Upper _)) mB listX =
                if fin then
                    fromBlocks_m . map (:[])  $ listX'
                else
                    solveTrsmAux alpha (trit mA) mB' listX'
                where
                    dim_mA = cantRows_m mA
                    (trit, mA) = unTriangT tritmA
                    mCantBlocksMN m = ceiling ((fromIntegral (cantRows_m m) / fromIntegral blockDim) :: Double)
                    invertBlockIndex i m = mCantBlocksMN m - i -1
                    listX' = mX_i:listX
                    i = length listX
                    fin = blockDim*(i + 1) >= dim_mA
                    torsoVerticalBlock m = subMatrix_m 0 0 (blockDim*(mCantBlocksMN m - 1)) (cantCols_m m) m
                    (mX_i, mB')
                        | null listX =
                            let
                                mA_00 = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) (invertBlockIndex 0 mA) mA
                                mB_0 = verticalBlock (getSqrBlockDim ctx) (invertBlockIndex 0 mB) mB
                                mB_sup = torsoVerticalBlock mB
                            in
                                (callTrsmBase trit ut alpha mA_00 mB_0, mB_sup)
                        | otherwise =
                            let
                                mX = head listX
                                mA_next = subMatrix_m
                                    0
                                    (blockDim * invertBlockIndex (i - 1) mA)
                                    (blockDim*(mCantBlocksMN mA - i))
                                    (min blockDim (dim_mA - (blockDim * invertBlockIndex (i - 1) mA))) mA
                                mB' = callGemmBase (negate (1/alpha)) mA_next mX 1 mB
                                mB'_sup = torsoVerticalBlock mB'
                                mA_ii = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) (invertBlockIndex i mA) mA
                                mB_i' = verticalBlock (getSqrBlockDim ctx) (invertBlockIndex 0 mB') mB'
                            in
                                (callTrsmBase trit ut alpha mA_ii mB_i', mB'_sup)
