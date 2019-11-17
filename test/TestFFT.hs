{-# LANGUAGE DataKinds #-}

module TestFFT where

import Protolude

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Curve.Weierstrass.BN254 (Fr)
import Data.Field.Galois
import Data.Pairing.BN254           (getRootOfUnity)
import FFT

data TestPoly f = TestPoly (CoeffVec f)
  deriving Show

testExp :: Int
testExp = 8

instance Arbitrary f => Arbitrary (TestPoly f) where
  arbitrary = TestPoly <$> vectorOf (2 ^ testExp) arbitrary

omega :: Fr
omega = getRootOfUnity testExp

unit_omega_is_primitive_root :: Assertion
unit_omega_is_primitive_root
  = assertBool "omega is not a correct primitive root of unity"
    $ isRootOfUnity (toU' omega :: RootsOfUnity 256 Fr)
    -- We are choosing 256 because there are n=2^8=256 roots of unity
    -- in this test case

prop_fftCorrect :: TestPoly Fr -> Bool
prop_fftCorrect (TestPoly coeffs)
  = dftNaive omega coeffs == fft getRootOfUnity coeffs

prop_inverseDftCorrect :: TestPoly Fr -> Bool
prop_inverseDftCorrect (TestPoly coeffs)
  = inverseDft getRootOfUnity (fft getRootOfUnity coeffs) == coeffs
    && fft getRootOfUnity (inverseDft getRootOfUnity coeffs) == coeffs
