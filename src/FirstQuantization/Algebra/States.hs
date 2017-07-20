{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FirstQuantization.Algebra.States where

import           FirstQuantization.Algebra.Types
import           Protolude

newtype QuantumState a = QuantumState {qState :: a -> Scalar}

mkState ::( a -> Scalar) -> QuantumState a
mkState f = QuantumState {qState = f}

instance Num (QuantumState a) where
  (+) f g = QuantumState {qState = \x -> qState f x + qState g x}
  (*) f g = QuantumState {qState = \x -> qState f x * qState g x}
  abs f = QuantumState {qState = abs . qState f}
  signum f = QuantumState {qState = signum . qState f}
  negate f = QuantumState {qState = negate . qState f}
  fromInteger x = QuantumState {qState = const $ fromInteger x}

instance Fractional (QuantumState a) where
  fromRational x = QuantumState {qState = const $ fromRational x}
  recip f = QuantumState {qState = recip . qState f}

instance Scalable (QuantumState a) Scalar where
  type ScalableType (QuantumState a) Scalar = QuantumState a
  (|*|) qS sc = QuantumState{ qState = const sc} * qS

instance Scalable Scalar (QuantumState a) where
  type ScalableType Scalar (QuantumState a) = QuantumState a
  (|*|) sc qS = QuantumState{ qState = const sc} * qS

instance QuantumUnit (QuantumState a) where
  hC f = QuantumState {qState = conjugate . qState f}

infixr 7 |>|
(|>|) :: (QuantumBasis a) => a -> QuantumState a -> Scalar
(|>|) st qS = qState qS st

infixr 7 |<|
(|<|) :: (QuantumBasis a) => QuantumState a -> a -> Scalar
(|<|) qS st = conjugate $ st |>| qS

infixr 7 |.|
(|.|) :: (QuantumBasis a) => QuantumState a -> QuantumState a -> Scalar
(|.|) qS1 qS2 = basisSum (\x -> (x |>| qS1) * (x |>| qS2))

infixr 7 |<>|
(|<>|) :: (QuantumBasis a) => QuantumState a -> QuantumState a -> Scalar
(|<>|) l r = hC l |.| r

normalize :: (QuantumBasis a) => QuantumState a -> QuantumState a
normalize qS =  (1/sqrt( qS |<>| qS )) |*| qS
