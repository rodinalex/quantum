{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Algebra where

import           Protolude

type Scalar = Complex Double

-- TYPECLASSES --

class QuantumBasis a where
  mkState :: a -> QuantumState a
  identityOperator :: QuantumOperator a

class QuantumUnit a where
  infixl 6 |+|
  (|+|) :: a -> a -> a
  infixl 6 |-|
  (|-|) :: a -> a -> a
  infixr 7 |*|
  (|*|) :: Scalar -> a -> a
  hC :: a -> a

class (QuantumUnit a) => BraKet a where
  infixr 7 |.|
  (|.|) :: a -> a -> Scalar
  infixr 7 |<>|
  (|<>|) :: a -> a -> Scalar
  (|<>|) a b = hC a |.| b
  normalize :: a -> a
  normalize x = (1/sqrt( x|<>|x )) |*| x

-- DEFINITION OF BRA AND KET --

newtype QuantumState a = QuantumState {qState :: a -> Scalar}

instance (QuantumBasis a) => QuantumUnit (QuantumState a) where
  (|+|) qS1 qS2 = QuantumState {qState = \x -> qState qS1 x + qState qS2 x}
  (|-|) qS1 qS2 = QuantumState {qState = \x -> qState qS1 x - qState qS2 x}
  (|*|) scalar qS = QuantumState {qState = (scalar *) <$> qState qS}
  hC qS = QuantumState {qState = conjugate <$> qState qS}

infixr 7 |><|
(|><|) :: (QuantumBasis a, BraKet (QuantumState a)) => QuantumState a -> QuantumState a -> QuantumOperator a
(|><|) l r = QuantumOperator{
              operator = \x -> (r |<>| mkState x) |*| l
            , operatorHC = \x -> (l |<>| mkState x) |*| r}

infixr 7 |>|
(|>|) :: a -> QuantumState a -> Scalar
(|>|) st qS = qState qS st

infixr 7 |<|
(|<|) :: QuantumState a -> a -> Scalar
(|<|) qS st = conjugate $ qState qS st

-- DEFINITION OF QUANTUM OPERATOR --

data QuantumOperator a = QuantumOperator {
                          operator   :: a -> QuantumState a
                        , operatorHC :: a -> QuantumState a}

instance (QuantumBasis a) => QuantumUnit (QuantumOperator a) where
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
(|>>|) :: (QuantumBasis a, BraKet (QuantumState a)) => (a -> QuantumState a) -> QuantumState a -> QuantumState a
(|>>|) f qS = QuantumState {qState = \x -> qS |.| f x}

infixr 9 |:|
(|:|) :: (QuantumBasis a, BraKet (QuantumState a)) => QuantumOperator a -> QuantumOperator a -> QuantumOperator a
(|:|) qO1 qO2 = QuantumOperator{
                  operator = \x -> operator qO1 |>>| operator qO2 x
                , operatorHC = \x -> operatorHC qO2 |>>| operatorHC qO1 x}

infixr 6 |->|
(|->|) :: (QuantumBasis a, BraKet (QuantumState a)) => QuantumOperator a -> QuantumState a -> QuantumState a
(|->|) qO qS = operator qO |>>| qS

infixl 6 |<-|
(|<-|) :: (QuantumBasis a, BraKet (QuantumState a)) => QuantumState a -> QuantumOperator a -> QuantumState a
(|<-|) qS qO = hC (hC qO |->| hC qS)

-- HILBERT SPACE EXTENSION --

-- infixr 8 :|:
-- data a :|: b = a :|: b
--
-- type family TensorProduct e :: *
--
-- type instance TensorProduct (a :|: b) = TensorProduct a :|: TensorProduct b
-- type instance TensorProduct (QuantumState a) = QuantumState a


-- data HilbertState a = QuantumState a :|: HilbertState b
-- ----------------
--
-- matrixForm :: (Ord a) => QuantumOperator a -> [Ket a] -> [[Amplitude]]
-- matrixForm op kets =
--   let
--     bras = fmap hC kets
--     elements =  fmap (fmap (|.|) bras <*> ) $ fmap (op |->|) <$> group kets
--   in
--     elements
--
-- commute :: (Ord a) => QuantumOperator a -> QuantumOperator a -> QuantumOperator a
-- commute a b = a |:| b |-| b |:| a
