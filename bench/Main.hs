{-# LANGUAGE NoImplicitPrelude #-}

-- To get the benchmarking data, run "stack bench".

module Main where

import Protolude

import Criterion.Main

import Poly

main :: IO ()
main = defaultMain
      [ bgroup "Polynomial operations" Poly.benchmarks
      ]
