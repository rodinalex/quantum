{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Algebra where

import           Data.Biapplicative
import           Protolude

-- AUXILIARY TYPES --

type Scalar = Complex Double

infixr 8 :|:
data a :|: b = a :|: b deriving (Show)

instance Bifunctor (:|:) where
  bimap f g (a :|: b) = f a :|: g b

instance Biapplicative (:|:) where
  bipure = (:|:)
  (<<*>>) (f:|:g) = bimap f g

-- TYPECLASSES --

class QuantumBasis a where
  mkState :: a -> QuantumState a
  infixr 7 |.|
  (|.|) :: QuantumState a -> QuantumState a -> Scalar

class QuantumUnit a where
  type Basis a
  infixl 6 |+|
  (|+|) :: a -> a -> a
  infixl 6 |-|
  (|-|) :: a -> a -> a
  infixr 7 |*|
  (|*|) :: Scalar -> a -> a
  hC :: a -> a

class QuantumApplication a b where
  infixr 6 |->|
  (|->|) :: a -> b -> b
  infixl 6 |<-|
  (|<-|) :: b -> a -> b

-- DEFINITION OF QUANTUM STATE --

newtype QuantumState a = QuantumState {qState :: a -> Scalar}

instance (QuantumBasis a) => QuantumUnit (QuantumState a) where
  type Basis (QuantumState a) = a
  (|+|) qS1 qS2 = QuantumState {qState = \x -> qState qS1 x + qState qS2 x}
  (|-|) qS1 qS2 = QuantumState {qState = \x -> qState qS1 x - qState qS2 x}
  (|*|) scalar qS = QuantumState {qState = (scalar *) <$> qState qS}
  hC qS = QuantumState {qState = conjugate <$> qState qS}

infixr 7 |<>|
(|<>|) :: (QuantumBasis a) => QuantumState a -> QuantumState a -> Scalar
(|<>|) l r = hC l |.| r

infixr 7 |><|
(|><|) :: (QuantumBasis a) => QuantumState a -> QuantumState a -> QuantumOperator a
(|><|) l r = QuantumOperator{
              operator = \x -> (r |<>| mkState x) |*| l
            , operatorHC = \x -> (l |<>| mkState x) |*| r}

infixr 7 |>|
(|>|) :: (QuantumBasis a) => a -> QuantumState a -> Scalar
(|>|) st qS = qState qS st

infixr 7 |<|
(|<|) :: (QuantumBasis a) => QuantumState a -> a -> Scalar
(|<|) qS st = conjugate $ qState qS st

normalize :: (QuantumBasis a) => QuantumState a -> QuantumState a
normalize qS = (1/sqrt( qS |<>| qS )) |*| qS

-- DEFINITION OF QUANTUM OPERATOR --

data QuantumOperator a = QuantumOperator {
                          operator   :: a -> QuantumState a
                        , operatorHC :: a -> QuantumState a}

instance (QuantumBasis a) => QuantumUnit (QuantumOperator a) where
    type Basis (QuantumOperator a) = a
    (|+|) qO1 qO2 = QuantumOperator{
                    operator = \x -> operator qO1 x |+| operator qO2 x
                  , operatorHC = \x -> operatorHC qO1 x |+| operatorHC qO2 x}
    (|-|) qO1 qO2 = QuantumOperator{
                    operator = \x -> operator qO1 x |-| operator qO2 x
                  , operatorHC = \x -> operatorHC qO1 x |-| operatorHC qO2 x}
    (|*|) scalar qO = QuantumOperator {
                      operator = (scalar |*|) <$> operator qO
                    , operatorHC = (conjugate scalar |*|) <$> operatorHC qO}
    hC qO = QuantumOperator {
            operator = operatorHC qO
          , operatorHC = operator qO}

infixr 8 |>>|
(|>>|) :: (QuantumBasis a, QuantumBasis b) => (b -> QuantumState a) -> QuantumState a -> QuantumState b
(|>>|) f qS = QuantumState {qState = \x -> qS |.| f x}

infixr 9 |:|
(|:|) :: (QuantumBasis a) => QuantumOperator a -> QuantumOperator a -> QuantumOperator a
(|:|) qO1 qO2 = QuantumOperator{
                  operator = \x -> operator qO1 |>>| operator qO2 x
                , operatorHC = \x -> operatorHC qO2 |>>| operatorHC qO1 x}

-- APPLICATION OF OPERATORS --

instance (QuantumBasis a, a ~ b) => QuantumApplication (QuantumOperator a) (QuantumState b) where
  (|->|) qO qS = operator qO |>>| qS
  (|<-|) qS qO = hC (hC qO |->| hC qS)

instance (QuantumBasis a, a ~ b, QuantumApplication c d) => QuantumApplication (QuantumOperator a :|: c) (QuantumState b :|: d) where
  (|->|) qO qS = bimap (|->|) (|->|) qO Data.Biapplicative.<<*>> qS
  (|<-|) qS qO = bimap (|<-|) (|<-|) qS Data.Biapplicative.<<*>> qO

-- BASIS CHANGE

newtype BasisTransform a b = BasisTransform {basisT :: b -> QuantumState a}

basisChange :: (QuantumBasis a, QuantumBasis b) => QuantumState a -> BasisTransform a b -> QuantumState b
basisChange qS tr = basisT tr |>>| qS

-- commute :: (Ord a) => QuantumOperator a -> QuantumOperator a -> QuantumOperator a
-- commute a b = a |:| b |-| b |:| a
