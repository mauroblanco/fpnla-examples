{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.TRSM.BlocksByRows () where

import           FPNLA.Matrix                               (cantCols_m,
                                                             cantRows_m,
                                                             diagonalBlock,
                                                             fromBlocks_m,
                                                             generate_m,
                                                             subMatrix_m,
                                                             verticalBlock)
import           FPNLA.Operations.BLAS                      (GEMM (gemm),
                                                             TRSM (trsm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (BlocksByRows, SqrBlockContext (getSqrBlockDim))
import           FPNLA.Operations.Parameters                (ResM, TransType (NoTrans, Trans, ConjTrans), TriangType (Upper, Lower),
                                                             blasResultM,
                                                             getResultDataM,
                                                             transTrans_m,
                                                             unTransT,
                                                             unTriangT, unUnitT)

instance (GEMM gs m v e, TRSM ts m v e) => TRSM (BlocksByRows gs ts) m v e where
    --trsm :: StratCtx s -> e -> TransType (TriangType (UnitType (m e))) -> m e -> Res s v m e
    trsm _ 0 _ mB = blasResultM $ generate_m (cantRows_m mB) (cantCols_m mB) (\_ _ -> 0)
    trsm (ctx, gctx, tctx) alpha tratmA mB = blasResultM $ solveTrsmAux alpha (tritAux (transTrans_m (trat mA))) mB undefined 0
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

            solveTrsmAux alpha tritmA mB mX i =
                if fin then
                    mX
                else
                    mX'
                where
                    (trit, mA) = unTriangT tritmA
                    dim_mA = cantRows_m mA
                    mCantBlocksMN = ceiling ((fromIntegral dim_mA / fromIntegral blockDim) :: Double)
                    invertBlockIndex i = case tritmA of
                        (Upper _) -> mCantBlocksMN - i - 1
                        (Lower _) -> i
                    fin = (i*blockDim) >= dim_mA
                    mX'
                        | i == 0 =
                            let
                                mA_00 = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) (invertBlockIndex 0) mA
                                mB_0 = verticalBlock (getSqrBlockDim ctx) (invertBlockIndex 0) mB
                                mX_0 = callTrsmBase trit ut alpha mA_00 mB_0
                            in
                                solveTrsmAux alpha (trit mA) mB mX_0 (i + 1)
                        | otherwise =
                            let
                                mA_ii = diagonalBlock (getSqrBlockDim ctx, getSqrBlockDim ctx) (invertBlockIndex i) mA
                                mB_i = verticalBlock (getSqrBlockDim ctx) (invertBlockIndex i) mB
                                mA_sub = case tritmA of
                                    (Upper _) -> subMatrix_m
                                        (blockDim * invertBlockIndex i)
                                        (blockDim*(invertBlockIndex i + 1))
                                        (min blockDim (dim_mA - blockDim * invertBlockIndex i))
                                        (min (blockDim*i) (dim_mA - blockDim*(invertBlockIndex i + 1)))
                                        mA
                                    (Lower _) -> subMatrix_m
                                        (blockDim*i)
                                        0
                                        (min blockDim (dim_mA - blockDim*i))
                                        (blockDim*i)
                                        mA
                                mB'_i = callGemmBase (negate (1/alpha)) mA_sub mX 1 mB_i
                                mX_i = callTrsmBase trit ut alpha mA_ii mB'_i
                                mX_join = case tritmA of
                                    (Upper _) -> fromBlocks_m [[mX_i], [mX]]
                                    (Lower _) -> fromBlocks_m [[mX], [mX_i]]
                            in
                                solveTrsmAux alpha (trit mA) mB mX_join (i + 1)
