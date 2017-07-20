{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FirstQuantization.Algebra.Types where

import           Protolude

type Scalar = Complex Double

class QuantumBasis a where
  basisSum :: (a -> Complex Double) -> Complex Double

class QuantumUnit a where
  hC :: a -> a

class Scalable a b where
  type ScalableType a b
  infixl 7 |*|
  (|*|) :: a -> b -> ScalableType a b

class QuantumApplication a b where
  infixr 6 |->|
  (|->|) :: a -> b -> b
  infixl 6 |<-|
  (|<-|) :: b -> a -> b
