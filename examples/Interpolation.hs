{-# LANGUAGE NoImplicitPrelude #-}

module Interpolation where

import Protolude

import Data.Curve.Weierstrass.BN254 (Fr)
import Data.Pairing.BN254           (getRootOfUnity)

import FFT

k :: Int
k = 5

polySize :: Int
polySize = 2^k

leftCoeffs, rightCoeffs :: [Fr]
leftCoeffs = map fromIntegral [1..polySize]
rightCoeffs = map fromIntegral (reverse [1..polySize])

main :: IO ()
main = do
  print $ interpolate getRootOfUnity leftCoeffs
  print $ fftMult getRootOfUnity leftCoeffs rightCoeffs
  pure ()
