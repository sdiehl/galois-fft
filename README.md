# galois-fft

Fast Fourier Transforms over finite fields. Provides functionality for
polynomial evaluation, polynomial interpolation, and computation of Lagrange
polynomials.

In a finite field F with 2^m elements. We can define a discrete Fourier
transform by selecting 2^m - 1 roots of unity ω ∈ F.

## Example


```haskell
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
```

## License

```
Copyright (c) 2018-2019 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
```
