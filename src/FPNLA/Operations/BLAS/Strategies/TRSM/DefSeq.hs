{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FPNLA.Operations.BLAS.Strategies.TRSM.DefSeq () where


import           FPNLA.Matrix                               (MatrixVector,
                                                             dim_m, elem_v,
                                                             fromCols_vm,
                                                             fromList_v,
                                                             toCols_vm)
import           FPNLA.Operations.BLAS                      (TRSM (trsm))
import           FPNLA.Operations.BLAS.Strategies.DataTypes (DefSeq)
import           FPNLA.Operations.Parameters                (Elt, TransType (NoTrans, Trans, ConjTrans), TriangType (Upper, Lower),
                                                             UnitType,
                                                             blasResultM,
                                                             dimTransUnit_m,
                                                             dimUnit_m,
                                                             elemTransUnit_m,
                                                             unTransT,
                                                             unTriangT)

forwardElim :: (Elt e, MatrixVector m v e) => e -> TransType (UnitType (m e)) -> v e -> v e
forwardElim alpha pmA vB = fromList_v res
    where
        cr = fst . dimTransUnit_m $ pmA -- Cantidad de ecuaciones
        res = solve cr -- Usada para la evaluacion perezosa
        solve 0 = []
        solve i = calcX i : solve (i-1)
        calcX i -- Calcula el elemento X_i
            | v_ind == 0  = 0 -- Devuelve 0 si el valor del vector vale 0.
            | otherwise   = (v_ind - sumValues 0 res) / elemTransUnit_m ind ind pmA
            where
                  ind = cr - i -- Indice real
                  ind_v = ind
                  eqr = cr - i -- Cantidad de ecuaciones resueltas
                  v_ind = alpha * elem_v ind_v vB
                  sumValues j (x:xs)
                      | j < eqr = x * elemTransUnit_m ind j pmA + sumValues (j+1) xs
                      | otherwise = 0
--
backwardElim :: (Elt e, MatrixVector m v e) => e -> TransType (UnitType (m e)) -> v e -> v e
backwardElim alpha pmA vB = fromList_v $ reverse res
    where
        cr = fst . dimTransUnit_m $ pmA -- Cantidad de ecuaciones
        res = solve cr -- Usada para la evaluaci√≥n perezosa
        solve 0 = []
        solve i = calcX i : solve (i-1)
        calcX i -- Calcula el elemento X_[cr - i]
            | v_ind == 0  = 0 -- Devuelve 0 si el valor del vector vale 0.
            | otherwise   = (v_ind - sumValues (cr-1) res) / elemTransUnit_m ind ind pmA
            where
                  ind = i - 1 -- Indice real
                  eqr = cr - i -- Cantidad de ecuaciones resueltas
                  v_ind = alpha * elem_v ind vB
                  sumValues j (x:xs)
                      | j > cr - 1 - eqr = x * elemTransUnit_m ind j pmA + sumValues (j-1) xs
                      | otherwise = 0
--
instance (Elt e, MatrixVector m v e) => TRSM DefSeq m v e where
    trsm _ alpha pmA mB
        | rowsmA /= colsmA = error "trsm: matrix A must be squared"
        | colsmA /= rowsmB = error "trsm: incompatible ranges"
        | otherwise = blasResultM . fromCols_vm . map (selectStrat pmA) $ (toCols_vm mB :: [v e])
        where
            (rowsmA, colsmA) = dimUnit_m . snd . unTriangT . snd . unTransT $ pmA
            rowsmB = fst . dim_m $ mB
            selectStrat (NoTrans (Lower umA)) = forwardElim alpha (NoTrans umA)
            selectStrat (Trans (Lower umA)) = backwardElim alpha (Trans umA)
            selectStrat (ConjTrans (Lower umA)) = backwardElim alpha (ConjTrans umA)
            selectStrat (NoTrans (Upper umA)) = backwardElim alpha (NoTrans umA)
            selectStrat (Trans (Upper umA)) = forwardElim alpha (Trans umA)
            selectStrat (ConjTrans (Upper umA)) = forwardElim alpha (ConjTrans umA)
