{-# LANGUAGE NoImplicitPrelude #-}

module Interpolation where

import Data.Curve.Weierstrass.BN254 (Fr)
import Data.Pairing.BN254 (getRootOfUnity)
import qualified Data.Vector as V
import FFT
import Protolude

k :: Int
k = 5

polySize :: Int
polySize = 2 ^ k

leftCoeffs, rightCoeffs :: V.Vector Fr
leftCoeffs  = V.generate polySize fromIntegral
rightCoeffs = V.generate polySize (fromIntegral . (polySize -))

main :: IO ()
main = do
  print $ interpolate getRootOfUnity leftCoeffs
  print $ fftMult getRootOfUnity leftCoeffs rightCoeffs
  pure ()
