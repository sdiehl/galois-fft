<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

# galois-fft

The library provides Fast Fourier Transforms over finite fields with
functionality for polynomial evaluation, polynomial interpolation, and
computation of Lagrange polynomials.

In a finite field <img src="/tex/2d4c6ac334688c42fb4089749e372345.svg?invert_in_darkmode&sanitize=true" align=middle width=10.045686749999991pt height=22.648391699999998pt/> with <img src="/tex/f46a5c3deaad97e010fdb1e011e7d93c.svg?invert_in_darkmode&sanitize=true" align=middle width=19.88405924999999pt height=21.839370299999988pt/> elements. We can define a discrete Fourier
transform by selecting <img src="/tex/1683127d6422f667f0fd702f2d9f7d89.svg?invert_in_darkmode&sanitize=true" align=middle width=49.01637509999998pt height=21.839370299999988pt/> roots of unity <img src="/tex/6040a26fde3c8575b728674f140bf80a.svg?invert_in_darkmode&sanitize=true" align=middle width=40.95873044999998pt height=22.648391699999998pt/>.

## Example

```haskell
import Protolude

import Data.Curve.Weierstrass.BN254 (Fr)
import Data.Pairing.BN254 (getRootOfUnity)

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
  print (interpolate getRootOfUnity leftCoeffs)
  print (fftMult getRootOfUnity leftCoeffs rightCoeffs)
  pure ()
```

## License

```
Copyright (c) 2018-2020 Adjoint Inc.

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
