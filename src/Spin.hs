{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Spin where

import           Algebra
import qualified Data.Map  as Map
import           Protolude

-- data CartesianDirection = X
--                         | Y
--                         | Z deriving (Eq,Show)
--
data Spin = SpinUp
          | SpinDown deriving (Eq,Show, Ord)

-- instance QuantumBasis Spin where
--   mkState s = QuantumState {qState = \x -> fromMaybe (0:+0) $ Map.lookup x $ Map.singleton s (1 :+ 0)}

-- instance BraKet (QuantumState Spin) where
--   (|.|) a b = 6

=======
-- qS1 :: QuantumState Spin
-- qS1 x =
--   case x of
--     SpinUp   -> 1
--     SpinDown -> -1
--
-- qS2 :: QuantumState Spin
-- qS2 x =
--   case x of
--     SpinUp   -> 3
--     SpinDown -> -3

--
-- zUp :: Ket Spin
-- zUp = mkKet (1 :+ 0) SpinUp
--
-- zDown :: Ket Spin
-- zDown = mkKet (1 :+ 0) SpinDown
--
-- xUp ::  Ket Spin
-- xUp = normalize (zUp |+| zDown)
--
-- xDown ::  Ket Spin
-- xDown = normalize (zUp |-| zDown)
--
-- yUp ::  Ket Spin
-- yUp = normalize (zUp |+| (0 :+ 1 :: Complex Double) |*| zDown)
--
-- yDown ::  Ket Spin
-- yDown = normalize (zUp |-| (0 :+ 1 :: Complex Double) |*| zDown)
--
-- jz :: QuantumKernel Spin
-- jz = QuantumKernel { kernel = f }
--   where
--   f x =
--     case x of
--       SpinUp   -> Map.singleton SpinUp (0.5 :+ 0)
--       SpinDown -> Map.singleton SpinDown ((-0.5) :+ 0)
--
-- jzOp :: QuantumOperator Spin
-- jzOp = QuantumOperator jz jz
