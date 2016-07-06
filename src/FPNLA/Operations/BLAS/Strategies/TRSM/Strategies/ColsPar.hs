{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.TRSM.Strategies.ColsPar () where

import           Control.DeepSeq                            (NFData)
import           Control.Parallel.Strategies                (parMap, rdeepseq)
import           FPNLA.Matrix                               (asColumn_vm, dim_m,
                                                             fromBlocks_m,
                                                             toCols_vm)
import           FPNLA.Operations.BLAS                      (TRSM (trsm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (ColsPar_ST)
import           FPNLA.Operations.Parameters                (ResM, blasResultM,
                                                             dimUnit_m,
                                                             getResultDataM,
                                                             unTransT,
                                                             unTriangT)

-- Optimizacion de la version secuencial resolviendo las columnas en paralelo:
instance (NFData (m e), TRSM s m v e) => TRSM (ColsPar_ST s) m v e where
    trsm ctx alpha pmA mB
        | rowsmA /= colsmA = error "trsm: matrix A must be squared"
        | colsmA /= rowsmB = error "trsm: incompatible ranges"
        | otherwise = blasResultM . fromBlocks_m . (:[]) . parMap rdeepseq (getResultDataM . callTrsmBase) $ toCols_vm mB
        where
            (rowsmA, colsmA) = dimUnit_m . snd . unTriangT . snd . unTransT $ pmA
            rowsmB = fst . dim_m $ mB
            callTrsmBase :: v e -> ResM s v m e
            callTrsmBase vB = trsm ctx alpha pmA $ asColumn_vm vB
