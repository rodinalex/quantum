{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Spin where

import           Algebra
import           Protolude

data Spin = SpinUp
          | SpinDown deriving (Eq,Show, Ord)

spinKernel :: (Scalar, Scalar) -> (Spin -> Scalar)
spinKernel ker x = case x of
                      SpinUp   -> fst ker
                      SpinDown -> snd ker

instance QuantumBasis Spin where
  basisSum f = sum $ map f [SpinUp, SpinDown]

zUp :: QuantumState Spin
zUp = mkState (spinKernel (1,0))

zDown :: QuantumState Spin
zDown = mkState (spinKernel (0,1))

xUp :: QuantumState Spin
xUp = normalize (zUp |+| zDown)

xDown :: QuantumState Spin
xDown = normalize (zUp |-| zDown)

yUp :: QuantumState Spin
yUp = normalize (zUp |+| (0 :+ 1) |*| zDown)

yDown :: QuantumState Spin
yDown = normalize (zUp |-| (0 :+ 1) |*| zDown)

pauliXKernel :: Spin -> QuantumState Spin
pauliXKernel x = case x of
                  SpinUp   -> zDown
                  SpinDown -> zUp

pauliYKernel :: Spin -> QuantumState Spin
pauliYKernel x = case x of
                  SpinUp   -> (0 :+ 1) |*| zDown
                  SpinDown -> (0 :+ (-1)) |*| zUp

pauliZKernel :: Spin -> QuantumState Spin
pauliZKernel x = case x of
                  SpinUp   -> zUp
                  SpinDown -> (-1) |*| zDown

pauliX :: QuantumOperator Spin
pauliX = QuantumOperator{
      operator = pauliXKernel
    , operatorHC = pauliXKernel
}

pauliY :: QuantumOperator Spin
pauliY = QuantumOperator{
      operator = pauliYKernel
    , operatorHC = fmap hC pauliYKernel
}

pauliZ :: QuantumOperator Spin
pauliZ = QuantumOperator{
      operator = pauliZKernel
    , operatorHC = pauliZKernel
}

jX :: QuantumOperator Spin
jX = 0.5 |*| pauliX

jY :: QuantumOperator Spin
jY = 0.5 |*| pauliY

jZ :: QuantumOperator Spin
jZ = 0.5 |*| pauliZ

jUp :: QuantumOperator Spin
jUp = jX |+| (0:+1) |*| jY

jDown :: QuantumOperator Spin
jDown = jX |-| (0:+1) |*| jY
