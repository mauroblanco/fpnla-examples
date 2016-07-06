{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module FPNLA.Utils (
    iif,
    mapPair,
    splitSlice,

    prevEnum,
    nextEnum,
    enumerate,
) where

mapPair :: (a -> b) -> (c -> d)-> (a, c) -> (b, d)
mapPair f g (a, c) = (f a,  g c)

-- inline if
iif :: Bool -> a -> a -> a
iif cond expTrue expFalse = if cond then expTrue else expFalse

-- Parte la lista parametro en trozos de tama√±o 'n'. El √∫ltimo puede ser mas peque√±o si no existe k tal que length ls = k * n.
splitSlice :: Int -> [a] -> [[a]]
splitSlice 0 _ = error "El tama√±o de n debe ser > 0"
splitSlice _ [] = []
splitSlice n ls = sl : splitSlice n sls
    where (sl, sls) = splitAt n ls

prevEnum :: (Eq a, Bounded a, Enum a) => a -> a
prevEnum a | a == minBound = maxBound
           | otherwise = pred a
nextEnum :: (Eq a, Bounded a, Enum a) => a -> a
nextEnum a | a == maxBound = minBound
           | otherwise = succ a

-- Devuelve una lista con los elementos de cualquier tipo que sea instancia de Enum y Bounded.
enumerate :: forall a. (Enum a, Bounded a) => [a]
enumerate = map toEnum [fromEnum (minBound :: a) .. fromEnum (maxBound :: a)]
