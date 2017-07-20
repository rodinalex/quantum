{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FirstQuantization.SpinHalf where

import           FirstQuantization.Algebra.Operators
import           FirstQuantization.Algebra.States
import           FirstQuantization.Algebra.Types
import           Protolude

data Spin = SpinUp
          | SpinDown deriving (Eq, Show)

instance QuantumBasis Spin where
  basisSum f = sum $ map f [SpinUp, SpinDown]

spinKernel :: (Scalar, Scalar) -> (Spin -> Scalar)
spinKernel ker x = case x of
                      SpinUp   -> fst ker
                      SpinDown -> snd ker

zUp :: QuantumState Spin
zUp = mkState (spinKernel (1,0))

zDown :: QuantumState Spin
zDown = mkState (spinKernel (0,1))

xUp :: QuantumState Spin
xUp = normalize (zUp + zDown)

xDown :: QuantumState Spin
xDown = normalize (zUp - zDown)

yUp :: QuantumState Spin
yUp = normalize (zUp + (0 :+ 1 :: Scalar) |*| zDown)

yDown :: QuantumState Spin
yDown = normalize (zUp - (0 :+ 1 :: Scalar) |*| zDown)

data PauliMatrix = PauliX
                 | PauliY
                 | PauliZ deriving (Eq, Show)

instance OperatorKernel PauliMatrix Spin where
    actOn pMatrix spin =
      case pMatrix of
        PauliX -> case spin of
          SpinUp   -> zDown
          SpinDown -> zUp
        PauliY -> case spin of
          SpinUp   -> (0 :+ 1 :: Scalar) |*| zDown
          SpinDown -> (0 :+ (-1) :: Scalar) |*| zUp
        PauliZ -> case spin of
          SpinUp   -> zUp
          SpinDown -> (-1 :: Scalar) |*| zDown

pauliX :: QuantumOperator Spin
pauliX = mkOperator (actOn PauliX)

pauliY :: QuantumOperator Spin
pauliY = mkOperator (actOn PauliY)

pauliZ :: QuantumOperator Spin
pauliZ = mkOperator (actOn PauliZ)

jX :: QuantumOperator Spin
jX = (0.5 :: Scalar) |*| pauliX

jY :: QuantumOperator Spin
jY = (0.5 :: Scalar) |*| pauliY

jZ :: QuantumOperator Spin
jZ = (0.5 :: Scalar) |*| pauliZ

jUp :: QuantumOperator Spin
jUp = jX + (0:+1 :: Scalar) |*| jY

jDown :: QuantumOperator Spin
jDown = jX - (0:+1 :: Scalar) |*| jY

rot :: QuantumOperator Spin
rot = (0 :+ 1 :: Scalar)|*| ((pi/2 :: Scalar) |*| jY)

rotRes :: Int -> Text
rotRes x = show (realPart $ (SpinUp |>|)$ expOpN rot zUp x :: Double)
