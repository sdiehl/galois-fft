{-# LANGUAGE NoImplicitPrelude #-}

module Poly (benchmarks) where

import Protolude

import Criterion.Main
import Data.Curve.Weierstrass.BN254 (Fr)
import Data.Pairing.BN254           (getRootOfUnity)

import FFT
import qualified Data.Vector as V

k :: Int
k = 5

polySize :: Int
polySize = 2^k

leftCoeffs, rightCoeffs :: V.Vector Fr
leftCoeffs = V.generate polySize fromIntegral
rightCoeffs = V.generate polySize (fromIntegral . (polySize -))

points :: V.Vector (Fr,Fr)
points
  = V.generate polySize (\i -> (getRootOfUnity k ^ i, fromIntegral i))

fftPoints :: V.Vector Fr
fftPoints = map snd points

benchmarks :: [Benchmark]
benchmarks
  = [ bench "FFT-based multiplication"
            $ nf (uncurry $ fftMult getRootOfUnity)
                 (leftCoeffs, rightCoeffs)
    , bench "FFT-based interpolation"
            $ nf (interpolate getRootOfUnity) fftPoints
    ]
