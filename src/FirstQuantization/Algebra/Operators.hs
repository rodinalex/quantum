{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FirstQuantization.Algebra.Operators where

import           FirstQuantization.Algebra.States
import           FirstQuantization.Algebra.Types
import           Protolude

newtype HermitianOperator a = HermitianOperator {hOperator :: a -> QuantumState a}

class OperatorKernel a b where
  actOn :: a -> b -> QuantumState b

infixr 8 |~>|
(|~>|) :: (QuantumBasis a, QuantumBasis b) => (a -> QuantumState b) -> QuantumState a -> QuantumState b
(|~>|) f qS = QuantumState {qState = \b -> basisSum (\a -> (a |>| qS) * (b |>| f a))}

type QuantumOperator a = [(HermitianOperator a, Scalar)]

mkOperator :: (a -> QuantumState a) -> QuantumOperator a
mkOperator knl = [(HermitianOperator {hOperator = knl}, 1)]

instance (QuantumBasis a) => Num (QuantumOperator a) where
  (+) = (++)
  (*) f g = fmap combine f <*> g
              where
                combine :: (QuantumBasis a) => (HermitianOperator a, Scalar) -> (HermitianOperator a, Scalar) -> (HermitianOperator a, Scalar)
                combine (f1, s1) (f2, s2) = (HermitianOperator {hOperator = \x -> hOperator f1 |~>| hOperator f2 x}, s1 * s2)
  abs = fmap (fmap abs)
  signum = fmap (fmap signum)
  negate = fmap (fmap negate)

instance QuantumUnit (QuantumOperator a) where
  hC = fmap (fmap conjugate)

infixr 9 |^|
(|^|) :: (QuantumBasis a) => QuantumOperator a -> Int -> QuantumOperator a
(|^|) qO 1 = qO
(|^|) qO n = qO * (qO |^| (n-1))

instance Scalable (QuantumOperator a) Scalar where
  type ScalableType (QuantumOperator a) Scalar = QuantumOperator a
  (|*|) qO sc = fmap (fmap (*sc)) qO

instance Scalable Scalar (QuantumOperator a) where
  type ScalableType Scalar (QuantumOperator a) = QuantumOperator a
  (|*|) sc = fmap (fmap (*sc))

infixr 7 |><|
(|><|) :: (QuantumBasis a) => QuantumState a -> QuantumState a -> QuantumOperator a
(|><|) l r = [(HermitianOperator {hOperator = \x -> (l |<| x) |*| r + (r |<| x) |*| l}, 1)
            , (HermitianOperator {hOperator = \x -> ( 0 :+ 1 :: Scalar) |*| ((l |<| x) |*| r - (r |<| x) |*| l)}, 0 :+ 1)]

-- APPLICATION OF OPERATORS --

instance (QuantumBasis a, a ~ b) => QuantumApplication (QuantumOperator a) (QuantumState b) where
  (|->|) qO qS = Protolude.sum $ fmap (\(f,scalar) -> scalar |*| (hOperator f |~>| qS)) qO
  (|<-|) qS qO = hC (hC qO |->| hC qS)

-- EXPONENTIAL APPLICATION

expOp :: (QuantumBasis a) => QuantumOperator a -> QuantumState a -> [QuantumState a]
expOp oP qS = zipWith (/) (iterate (oP |->|) qS) ( fromRational <$> scanl (*) 1 ([1,2..]::[Rational]))

expOpN :: (QuantumBasis a) => QuantumOperator a -> QuantumState a -> Int -> QuantumState a
expOpN oP qS n = sum $ take n $ expOp oP qS

newtype BasisTransform a b = BasisTransform {basisT :: a -> QuantumState b}

basisChange :: (QuantumBasis a, QuantumBasis b) => QuantumState a -> BasisTransform a b -> QuantumState b
basisChange qS tr = basisT tr |~>| qS
