module Main where

import           Criterion.Main
import           FirstQuantization.SpinHalf
import           Lib
import           Protolude

main :: IO ()
main = defaultMain [
  bgroup "rotRes" [ bench "1" $ whnf rotRes 1
                  , bench "2" $ whnf rotRes 2
                  , bench "4" $ whnf rotRes 4
                  , bench "8" $ whnf rotRes 8
                  , bench "16" $ whnf rotRes 16
                  -- , bench "32" $ whnf rotRes 32
                  ]
  ]
