mandelbrot :: (Real, Real) -> Integer -> Integer
mandelbrot (x, y) maxIterations =
    let
        maxRadius = 2.0
        z = (0.0, 0.0)
        iterateMandelbrot c z n =
            if n == maxIterations || magnitude z > maxRadius
                then n
                else iterateMandelbrot c (addComplex (squareComplex z) c) (n + 1)
    in
        iterateMandelbrot (x, y) z 0

squareComplex :: (Real, Real) -> (Real, Real)
squareComplex (a, b) = (a^2 - b^2, 2*a*b)

addComplex :: (Real, Real) -> (Real, Real) -> (Real, Real)
addComplex (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

magnitude :: (Real, Real) -> Real
magnitude (a, b) = sqrt (a^2 + b^2)