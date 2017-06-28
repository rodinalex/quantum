{-# LANGUAGE TypeFamilies #-}

module Spin where

import           BraKet
import qualified Data.Map  as Map
import           Protolude

-- data CartesianDirection = X
--                         | Y
--                         | Z deriving (Eq,Show)
--
data Spin = SpinUp
          | SpinDown deriving (Eq,Show, Ord)

zUp :: Ket Spin
zUp = mkKet (1 :+ 0) SpinUp

zDown :: Ket Spin
zDown = mkKet (1 :+ 0) SpinDown

xUp ::  Ket Spin
xUp = normalize (zUp |+| zDown)

xDown ::  Ket Spin
xDown = normalize (zUp |-| zDown)

yUp ::  Ket Spin
yUp = normalize (zUp |+| (0 :+ 1 :: Complex Double) |*| zDown)

yDown ::  Ket Spin
yDown = normalize (zUp |-| (0 :+ 1 :: Complex Double) |*| zDown)

jz :: QuantumKernel Spin
jz = QuantumKernel { kernel = f }
  where
  f x =
    case x of
      SpinUp   -> Map.singleton SpinUp (0.5 :+ 0)
      SpinDown -> Map.singleton SpinDown ((-0.5) :+ 0)

jzOp :: QuantumOperator Spin
jzOp = QuantumOperator jz jz

--
-- data Jz = Jz
--
-- instance OperatorClass Jz where
--   type StateType Jz = Spin
--   hC Jz = Jz
--   qO Jz s =
--     case s of
--       SpinUp   -> Map.singleton SpinUp (0.5 :+ 0)
--       SpinDown -> Map.singleton SpinDown ((-0.5) :+ 0)
