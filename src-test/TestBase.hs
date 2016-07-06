{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module TestBase(
    GEMMTestData(..), 
    StrassenTestData(..), 
    TRSMTestData(..), 
    PotrfTestData(..), 
    Tag2(), 
    eqMatrix, 
    handleResult,
    Truncable(..)
    
) where

import           Data.Complex (Complex ((:+)))
import Test.QuickCheck (Result(..), Arbitrary(arbitrary), Testable(property), Gen, choose, vectorOf, elements)
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Default (Default(def))
import System.Random (Random)
import Debug.Trace (trace)
import Control.Applicative (liftA)

import FPNLA.Matrix
       (Matrix(fromList_m, dim_m, elem_m),
        Matrix(generate_m, transpose_m, zipWith_m))
import FPNLA.Matrix.Instances.HMatrix ()
import FPNLA.Operations.Parameters (Elt, TriangType(..), TransType(..), UnitType(..), StratCtx)
import FPNLA.Operations.LAPACK.Strategies (HMatrixBindSeq, CholLLVSeq, CholLLVPar_Repa, CholLLVBlocksSeq, newNullContext, newSqrBlockContext)
import FPNLA.Operations.BLAS.Strategies (DefPar_ST, CBindSeq, DefSeq, BlocksSeq, BlocksByRows, BlocksByCols, DefPar_MP, BlocksPar_MP, StrassenPar_MP, ColsPar_MP, DefPar_ST, DefPar_Acc, BlocksPar_ST, StrassenPar_ST, ColsPar_ST, newBlockContext, newStrassenContext)
import FPNLA.Utils (iif)

-- Links interesantes:
-- http://www.haskell.org/haskellwiki/Scoped_type_variables
-- http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/other-type-extensions.html seccion 7.11.7
-- http://hackage.haskell.org/trac/ghc/ticket/3485

blockSize :: Int
blockSize = 2

-- Valor de epsilon para hacer las comparaciones:
epsilon :: Double
epsilon = 1e-5

class Truncable a where
    inEpsilonRange :: Real e => e -> a -> a -> Bool

instance (Floating n, RealFrac n) => Truncable n where
    inEpsilonRange eps n1 n2 = expN1 == expN2 && abs(mantN1 - mantN2) <= epsFractional
        where
            epsFractional = fromRational $ toRational eps
            shift n d1 = n * (10 ** fromIntegral d1)
            getExponent r
                    | r == 0 = 0
                    | r < 0 = getExponent (abs r)
                    | r > 10 = 1 + getExponent (r / 10)
                    | r < 1 = (-1) + getExponent (r * 10)
                    | otherwise = 0
            expN1 :: Integer = getExponent n1
            expN2 :: Integer = getExponent n2
            mantN1 = shift n1 (-expN1)
            mantN2 = shift n2 (-expN2)

--
instance (RealFrac n, Truncable n) => Truncable (Complex n) where
    inEpsilonRange eps (r1:+i1) (r2:+i2) = inEpsilonRange eps r1 r2 && inEpsilonRange eps i1 i2


-- Compara una matriz elemento a elemento truncando los d√≠gitos. Si falla imprime los √≠ndices del primer elemento que fall√≥:
eqMatrix :: (Show e, Eq e, Truncable e, Matrix m e) => m e -> m e -> Bool
eqMatrix m1 m2 = dimm1 == dimm2 && eqElems 0 0
    where
        dimm1 = dim_m m1
        dimm2 = dim_m m2
        (m,n) = dimm1
        getElem = {--truncDigits truncValue $--} elem_m
        eqElems i j
          | j >= n = eqElems (i+1) 0
          | i >= m = True
          -- | getElem i j m1 /= getElem i j m2 = trace ("(i,j) = (" ++ show i ++ "," ++ show j ++ ")") False
          | not $ inEpsilonRange epsilon (getElem i j m1) (getElem i j m2) = trace ("(i,j) = (" ++ show i ++ "," ++ show j ++ ")" ++ " / m1 = " ++ show (getElem i j m1) ++ " / m2 = " ++ show (getElem i j m2)) False
          | otherwise =  eqElems i (j+1)

arbSDPMatrix :: (Elt e, Arbitrary e, Random e, Matrix m e) => Int -> Gen (m e)
arbSDPMatrix n = do
    la <- vectorOf (n*n) $ choose (0, 1)
    ld <- vectorOf n $ choose (100, 200)
    let a = fromList_m n n la
        d = generate_m n n (\i j -> iif (i == j) (ld !! i) 0)
        s = zipWith_m (+) a (transpose_m a)
        r = zipWith_m (+) s d
    return r


handleResult :: Monad m => Result -> m ()
handleResult (Success{}) = return ()
handleResult (GaveUp{}) = fail "GaveUp"
handleResult (Failure{}) = fail "Failure"
handleResult (NoExpectedFailure{}) = fail "NoExpectedFailure"

--
instance Arbitrary (TransType a) where
    arbitrary = elements [NoTrans undefined, Trans undefined, ConjTrans undefined]
--
instance Arbitrary (a -> TransType a) where
    arbitrary = elements [NoTrans, Trans, ConjTrans]
instance Arbitrary (a -> TriangType a) where
    arbitrary = elements [Lower, Upper]
instance Arbitrary (a -> UnitType a) where
    arbitrary = elements [Unit, NoUnit]

--
arbitraryIntBounded :: Gen Int
arbitraryIntBounded = choose (2, 10)

arbitraryMatrix :: (Arbitrary e, Matrix m e) => Int -> Int -> Gen (m e)
arbitraryMatrix m n = do
    l <- vectorOf (m*n) arbitrary
    return $ fromList_m m n l

arbitraryTransMatrix :: (Arbitrary e, Matrix m e) => TransType a -> Int -> Int -> Gen (TransType (m e))
arbitraryTransMatrix (NoTrans _) m n = liftA NoTrans $ arbitraryMatrix m n
arbitraryTransMatrix (Trans _) m n = liftA Trans $ arbitraryMatrix n m
arbitraryTransMatrix (ConjTrans _) m n = liftA ConjTrans $ arbitraryMatrix n m


data GEMMTestData m e = GEMMTestData (TransType (m e)) (TransType (m e)) e e (m e) deriving (Show)
data StrassenTestData m e = StrassenTestData (TransType (m e)) (TransType (m e)) e e (m e) deriving (Show)
data TRSMTestData m e = TRSMTestData e (TransType (TriangType (UnitType (m e)))) (m e) deriving (Show)

instance (Matrix m e, Arbitrary e) => Arbitrary (GEMMTestData m e) where
    arbitrary = do
        m <- arbitraryIntBounded
        n <- arbitraryIntBounded
        k <- arbitraryIntBounded
        ttmA <- arbitrary
        ttmB <- arbitrary
        pmA <- arbitraryTransMatrix ttmA m k
        pmB <- arbitraryTransMatrix ttmB k n
        alpha <- arbitrary
        beta <- arbitrary
        mC <- arbitraryMatrix m n
        return $ GEMMTestData pmA pmB alpha beta mC

instance (Matrix m e, Arbitrary e) => Arbitrary (StrassenTestData m e) where
    arbitrary = do
        e <- choose (2, 4) :: Gen Int
        let n = truncate $ (2 :: Double) ** fromIntegral e
        ttmA <- arbitrary
        ttmB <- arbitrary
        alpha <- arbitrary
        mA <- arbitraryTransMatrix ttmA n n
        mB <- arbitraryTransMatrix ttmB n n
        beta <- arbitrary
        mC <- arbitraryMatrix n n
        return $ StrassenTestData mA mB alpha beta mC
--
instance (Matrix m e, Arbitrary e) => Arbitrary (TRSMTestData m e) where
    arbitrary = do
        m <- arbitraryIntBounded
        n <- arbitraryIntBounded
        trans <- arbitrary :: Gen (a -> TransType a)
        triang <- arbitrary :: Gen (a -> TriangType a)
        unit <- arbitrary :: Gen (a -> UnitType a)
        alpha <- arbitrary
        mA <- arbitraryMatrix m m
        mB <- arbitraryMatrix m n
        return $ TRSMTestData alpha (trans . triang . unit $ mA) mB

data PotrfTestData m e = PotrfTestData (TriangType (m e)) deriving (Show)

instance (Matrix m e, Elt e, Arbitrary e, Random e) => Arbitrary (PotrfTestData m e) where
    arbitrary = do
        _ <- arbitrary :: Gen (a -> TriangType a)
        n <- choose (3, 5)
        m <- arbSDPMatrix (blockSize ^ (n :: Int))
        return $ PotrfTestData $ Lower m

-- Cualquier propiedad tageada es una propiedad:
instance (Testable p) => Testable (Tagged t p) where
    property p = property $ unTagged p
    
data Tag2 (s :: *) (v :: * -> *)

instance (StratCtx HMatrixBindSeq ~ ctx_Seq) => Default (Tagged HMatrixBindSeq ctx_Seq) where
    def = Tagged newNullContext

instance (StratCtx dots ~ ctx_dots, Default (Tagged dots ctx_dots), StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (CholLLVSeq dots gs) ~ ctx_bp) => Default (Tagged (CholLLVSeq dots gs) ctx_bp) where
    def = Tagged (unTagged (def :: Tagged dots ctx_dots), unTagged (def :: Tagged gs ctx_gs))

instance (StratCtx dots ~ ctx_dots, Default (Tagged dots ctx_dots), StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (CholLLVPar_Repa dots gs) ~ ctx_bp) => Default (Tagged (CholLLVPar_Repa dots gs) ctx_bp) where
    def = Tagged (unTagged (def :: Tagged dots ctx_dots), unTagged (def :: Tagged gs ctx_gs))

instance (StratCtx ps ~ ctx_ps, Default (Tagged ps ctx_ps), StratCtx ss ~ ctx_ss, Default (Tagged ss ctx_ss), StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx ts ~ ctx_ts, Default (Tagged ts ctx_ts), StratCtx (CholLLVBlocksSeq ps ss gs ts) ~ ctx_cs) => Default (Tagged (CholLLVBlocksSeq ps ss gs ts) ctx_cs) where
    def = Tagged (newSqrBlockContext blockSize, unTagged (def :: Tagged ps ctx_ps), unTagged (def :: Tagged ss ctx_ss), unTagged (def :: Tagged gs ctx_gs), unTagged (def :: Tagged ts ctx_ts))


--
instance (StratCtx DefSeq ~ ctx_DefSeq) => Default (Tagged DefSeq ctx_DefSeq) where
    def = Tagged newNullContext
instance (StratCtx CBindSeq ~ ctx_DefSeq) => Default (Tagged CBindSeq ctx_DefSeq) where
    def = Tagged newNullContext
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (BlocksSeq gs) ~ ctx_bp) => Default (Tagged (BlocksSeq gs) ctx_bp) where
    def = Tagged (newBlockContext 2 2, unTagged (def :: Tagged gs ctx_gs))
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx ts ~ ctx_ts, Default (Tagged ts ctx_ts), StratCtx (BlocksByCols gs ts) ~ ctx_bbcp) => Default (Tagged (BlocksByCols gs ts) ctx_bbcp) where
    def = Tagged (newSqrBlockContext 2, unTagged (def :: Tagged gs ctx_gs), unTagged (def :: Tagged ts ctx_ts))
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx ts ~ ctx_ts, Default (Tagged ts ctx_ts), StratCtx (BlocksByRows gs ts) ~ ctx_bbcp) => Default (Tagged (BlocksByRows gs ts) ctx_bbcp) where
    def = Tagged (newSqrBlockContext 2, unTagged (def :: Tagged gs ctx_gs), unTagged (def :: Tagged ts ctx_ts))

instance (StratCtx DefPar_MP ~ ctx_DefPar) => Default (Tagged DefPar_MP ctx_DefPar) where
    def = Tagged newNullContext
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (BlocksPar_MP gs) ~ ctx_bp) => Default (Tagged (BlocksPar_MP gs) ctx_bp) where
    def = Tagged (newBlockContext 2 2, unTagged (def :: Tagged gs ctx_gs))
instance (StratCtx ts ~ ctx_ts, Default (Tagged ts ctx_ts), StratCtx (ColsPar_MP ts) ~ ctx_cp) => Default (Tagged (ColsPar_MP ts) ctx_cp) where
    def = Tagged $ unTagged (def :: Tagged ts ctx_ts)
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (StrassenPar_MP gs) ~ ctx_bp) => Default (Tagged (StrassenPar_MP gs) ctx_bp) where
   def = Tagged (newStrassenContext 2, unTagged (def :: Tagged gs ctx_gs))

instance (StratCtx DefPar_ST ~ ctx_DefPar) => Default (Tagged DefPar_ST ctx_DefPar) where
    def = Tagged newNullContext
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (BlocksPar_ST gs) ~ ctx_bp) => Default (Tagged (BlocksPar_ST gs) ctx_bp) where
    def = Tagged (newBlockContext 2 2, unTagged (def :: Tagged gs ctx_gs))
instance (StratCtx ts ~ ctx_ts, Default (Tagged ts ctx_ts), StratCtx (ColsPar_ST ts) ~ ctx_cp) => Default (Tagged (ColsPar_ST ts) ctx_cp) where
    def = Tagged $ unTagged (def :: Tagged ts ctx_ts)
instance (StratCtx gs ~ ctx_gs, Default (Tagged gs ctx_gs), StratCtx (StrassenPar_ST gs) ~ ctx_bp) => Default (Tagged (StrassenPar_ST gs) ctx_bp) where
   def = Tagged (newStrassenContext 2, unTagged (def :: Tagged gs ctx_gs))

instance (StratCtx DefPar_Acc ~ ctx_DefPar) => Default (Tagged DefPar_Acc ctx_DefPar) where
    def = Tagged newNullContext


