{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FPNLA.Operations.BLAS.Strategies.DataTypes (
    DefSeq,
    CBindSeq,
    BlocksByCols,
    BlocksByRows,
    BlocksSeq,
    StrassenSeq,

    DefPar_FIO,

    DefPar_ST,
    ColsPar_ST,
    StrassenPar_ST,
    BlocksPar_ST,

    DefPar_MP,
    ColsPar_MP,
    StrassenPar_MP,
    BlocksPar_MP,

    DefPar_Acc,
    DefPar_AccGen,

    StrassenContext(getStrassenLimit),

    newStrassenContext,

    NullContext,
    BlockContext(..),
    SqrBlockContext(..)

) where

import           FPNLA.Operations.Parameters (StratCtx)
import           FPNLA.Operations.Utils (NullContext, BlockContext(..), SqrBlockContext(..))

-- Contextos:
data StrassenContext = StrassenContext {
        getStrassenExponentialLimit :: Int,
        getStrassenLimit            :: Int
    } deriving (Show)

newStrassenContext :: Int -> StrassenContext
newStrassenContext e = StrassenContext { getStrassenExponentialLimit = e, getStrassenLimit = 2 ^ e }

-- Estrategias:
data DefSeq
data CBindSeq
data BlocksSeq gs
data BlocksByCols gs ts
data BlocksByRows gs ts
data StrassenSeq gs

-- ForkIO
data DefPar_FIO

-- Strategies
data DefPar_ST
data ColsPar_ST s
data StrassenPar_ST gs
data BlocksPar_ST gs

-- Monad-Par
data DefPar_MP
data ColsPar_MP s
data StrassenPar_MP gs
data BlocksPar_MP gs

data DefPar_Acc
data DefPar_AccGen

type instance StratCtx DefSeq = NullContext
type instance StratCtx CBindSeq = NullContext
type instance StratCtx (BlocksByCols gs ts) = (SqrBlockContext, StratCtx gs, StratCtx ts)
type instance StratCtx (BlocksByRows gs ts) = (SqrBlockContext, StratCtx gs, StratCtx ts)
type instance StratCtx (BlocksSeq gs) = (BlockContext, StratCtx gs)
type instance StratCtx (StrassenSeq gs)= (StrassenContext, StratCtx gs)

type instance StratCtx DefPar_FIO = NullContext

type instance StratCtx DefPar_ST = NullContext
type instance StratCtx (ColsPar_ST s) = StratCtx s
type instance StratCtx (StrassenPar_ST gs)= (StrassenContext, StratCtx gs)
type instance StratCtx (BlocksPar_ST gs) = (BlockContext, StratCtx gs)

type instance StratCtx DefPar_MP = NullContext
type instance StratCtx (ColsPar_MP s) = StratCtx s
type instance StratCtx (StrassenPar_MP gs)= (StrassenContext, StratCtx gs)
type instance StratCtx (BlocksPar_MP gs) = (BlockContext, StratCtx gs)

type instance StratCtx DefPar_Acc = NullContext
type instance StratCtx DefPar_AccGen = NullContext

