{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FirstQuantization.Algebra.HilbertExtension where

import           Data.Biapplicative
import           FirstQuantization.Algebra.Operators
import           FirstQuantization.Algebra.States
import           FirstQuantization.Algebra.Types
import           Protolude

-- infixr 8 :|:
-- data a :|: b = a :|: b deriving (Show)
--
-- instance Bifunctor (:|:) where
--   bimap f g (a :|: b) = f a :|: g b
--
-- instance Biapplicative (:|:) where
--   bipure = (:|:)
--   (<<*>>) (f:|:g) = bimap f g
--
-- instance (QuantumBasis a, a ~ b, QuantumApplication c d) => QuantumApplication (QuantumOperator a :|: c) (QuantumState b :|: d) where
--   (|->|) qO qS = bimap (|->|) (|->|) qO Data.Biapplicative.<<*>> qS
--   (|<-|) qS qO = bimap (|<-|) (|<-|) qS Data.Biapplicative.<<*>> qO
