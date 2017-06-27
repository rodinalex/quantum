{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module BraKet where
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Protolude

-- TYPECLASSES --

class QuantumUnit a where
  infixl 6 |+|
  (|+|) :: a -> a -> a
  infixl 6 |-|
  (|-|) :: a -> a -> a
  infixl 7 |*|
  (|*|) :: Amplitude -> a -> a

class QuantumStructure a where
  type ConjType a
  hC :: a -> ConjType a

class BraKet a where
  normalize :: a -> a
-----------------

type Amplitude = Complex Double

-- DEFINITION OF BRA AND KET --

type QuantumState a = Map a Amplitude

instance (Ord a) => QuantumUnit (QuantumState a) where
  (|+|) = Map.unionWith (+)
  (|-|) x y = Map.unionWith (+) x (Map.map ((-1) *) y)
  (|*|) cd = Map.map (cd * )

data Bra a = Bra {bra :: QuantumState a} deriving (Eq,Show)
data Ket a = Ket {ket :: QuantumState a} deriving (Eq,Show)

mkKet :: Amplitude -> a -> Ket a
mkKet amp st = Ket {ket = Map.singleton st amp}

instance (Ord a) => QuantumUnit (Bra a) where
    (|+|) x y = Bra { bra = bra x |+| bra y }
    (|-|) x y = Bra { bra = bra x |-| bra y }
    (|*|) cd st = Bra { bra = cd |*| bra st }

instance (Ord a) => QuantumUnit (Ket a) where
    (|+|) x y = Ket { ket = ket x |+| ket y }
    (|-|) x y = Ket { ket = ket x |-| ket y }
    (|*|) cd st = Ket { ket = cd |*| ket st }

instance (Ord a) => QuantumStructure (Bra a) where
  type ConjType (Bra a) = Ket a
  hC br = Ket (conjugate <$> bra br)

instance (Ord a) => QuantumStructure (Ket a) where
  type ConjType (Ket a) = Bra a
  hC kt = Bra (conjugate <$> ket kt)

instance (Ord a) => BraKet (Bra a) where
  normalize br =
    let
      n = 1/sqrt(br |.| hC br)
    in
      n |*| br

instance (Ord a) => BraKet (Ket a) where
  normalize kt =
    let
      n = 1/sqrt(hC kt |.| kt)
    in
      n |*| kt

-- DEFINITION OF QUANTUM OPERATOR --

newtype QuantumKernel a = QuantumKernel {kernel :: a -> QuantumState a}

instance (Ord a) => QuantumUnit (QuantumKernel a) where
  (|+|) qK1 qK2 = QuantumKernel {kernel = \x -> kernel qK1 x |+| kernel qK2 x}
  (|-|) qK1 qK2 = QuantumKernel {kernel = \x -> kernel qK1 x |-| kernel qK2 x}
  (|*|) cd qK = QuantumKernel {kernel = fmap (Map.map (cd *)) (kernel qK)}

infixr 7 |<>|
(|<>|) :: (Ord a) => QuantumKernel a -> QuantumState a -> QuantumState a
(|<>|) kr st = Protolude.foldl (|+|) Map.empty $ (\(x, amp) -> amp |*| kernel kr x) <$> Map.toList st

data QuantumOperator a = QuantumOperator {
                         operator   :: QuantumKernel a
                       , operatorHC :: QuantumKernel a
                         }

instance (Ord a) => QuantumUnit (QuantumOperator a) where
  (|+|) x y = QuantumOperator {operator = operator x |+| operator y
                             , operatorHC = operatorHC x |+| operatorHC y}
  (|-|) x y = QuantumOperator {operator = operator x |-| operator y
                             , operatorHC = operatorHC x |-| operatorHC y}
  (|*|) cd qO = QuantumOperator { operator = cd |*| operator qO
                                , operatorHC = conjugate cd |*| operatorHC qO}

instance QuantumStructure (QuantumOperator a) where
  type ConjType (QuantumOperator a) = QuantumOperator a
  hC qO = QuantumOperator { operator = operatorHC qO
                          , operatorHC = operator qO}

infixr 6 |->|
(|->|) :: (Ord a) => QuantumOperator a -> Ket a -> Ket a
(|->|) qO kt = Ket {ket = operator qO |<>| ket kt}

infixl 6 |<-|
(|<-|) :: (Ord a) => Bra a -> QuantumOperator a -> Bra a
(|<-|) br qO = hC (hC qO |->| hC br)

infixr 9 |:|
(|:|) :: (Ord a) => QuantumOperator a -> QuantumOperator a -> QuantumOperator a
(|:|) qO1 qO2 =
    QuantumOperator {
      operator = QuantumKernel (\x -> operator qO1 |<>| (kernel $ operator qO2) x)
    , operatorHC = QuantumKernel (\x -> operatorHC qO2 |<>| (kernel $ operatorHC qO1) x)}

matrixForm :: (Ord a) => QuantumOperator a -> [Ket a] -> [Amplitude]
matrixForm op kets =
  let
    bras = fmap hC kets
    elements = fmap (|.|) bras <*> fmap (op |->| ) kets
  in
    elements
-------------------------------

-- DOT PRODUCT --
infixr 7 |.|
(|.|) :: (Ord a) => Bra a -> Ket a -> Complex Double
(|.|) x y = Map.foldr (+) 0 (Map.intersectionWith (*) (bra x) (ket y))
-----------------
