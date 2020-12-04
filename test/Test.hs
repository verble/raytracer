
module Test where

import Control.Monad
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import System.Exit

import Canvas
import Matrix

assertFloatsEq :: String -> Double -> Double -> Assertion
assertFloatsEq preface d1 d2 = unless (floatsEq d1 d2) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected: " ++ show d2 ++ "\n but got: " ++ show d1

-- chapter 1 tests, tuples.feature

-- Scenario: A tuple with w=1.0 is a point
testTuples01 = TestCase $ do
    let t = tuple 4.3 (-4.2) 3.1 1.0
    assertEqual "for (x tuple)" 4.3  (x t)
    assertEqual "for (y tuple)" (-4.2) (y t)
    assertEqual "for (z tuple)" 3.1  (z t)
    assertEqual "for (w tuple)" 1.0  (w t)
    assertBool "for (isPoint t)" (isPoint t)
    assertBool "for (not $ isVector t)" (not $ isVector t)

-- Scenario: A tuple with w=0 is a vector
testTuples02 = TestCase $ do
    let t = tuple 4.3 (-4.2) 3.1 0.0
    assertEqual "for (x tuple)" 4.3  (x t)
    assertEqual "for (y tuple)" (-4.2) (y t)
    assertEqual "for (z tuple)" 3.1  (z t)
    assertEqual "for (w tuple)" 0.0  (w t)
    assertBool "for (not $ isPoint t)" (not $ isPoint t)
    assertBool "for (isVector t)" (isVector t)

-- Scenario: point() creates tuples with w=1
testTuples03 = TestCase $ do
    let p = point 4 (-4) 3
    assertEqual "for (point 4 (-4) 3)" p (tuple 4 (-4) 3 1)

-- Scenario: vector() creates tuples with w=0
testTuples04 = TestCase $ do
    let v = vector 4 (-4) 3
    assertEqual "for (vector 4 (-4) 3)" v (tuple 4 (-4) 3 0)

-- Scenario: Adding two tuples
testTuples05 = TestCase $ do
    let a1 = tuple 3 (-2) 5 1
    let a2 = tuple (-2) 3 1 0
    assertEqual "for ((addTuples a1 a2) == tuple 1 1 6 1)"
        (addTuples a1 a2) (tuple 1 1 6 1)

-- Scenario: Subtracting two points
testTuples06 = TestCase $ do
    let p1 = point 3 2 1
    let p2 = point 5 6 7
    assertEqual "for ((subTuples p1 p2) == vector -2 -4 -6)"
        (subTuples p1 p2) (vector (-2) (-4) (-6))

-- Scenario: Subtracting a vector from a point
testTuples07 = TestCase $ do
    let p = point 3 2 1
    let v = vector 5 6 7
    assertEqual "for (subTuples p v) == point -2 -4 -6"
        (subTuples p v) (point (-2) (-4) (-6))

-- Scenario: Subtracting a vector from the zero vector
testTuples08 = TestCase $ do
    let v = vector 1 (-2) 3
    assertEqual "for (subTuples z v) == vector -1 2 -3"
        (subTuples zero4x1 v) (vector (-1) 2 (-3))

-- Scenario: Negating a tuple
testTuples09 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertEqual "for (negTuple a) == tuple -1 2 -3 4"
        (negTuple a) (tuple (-1) 2 (-3) 4)

-- Scenario: Multiplying a tuple by a scalar
testTuples10 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertEqual "for (multScalar a 3.5) == tuple 3.5 -7 10.5 -14)"
        (multScalar a 3.5) (tuple 3.5 (-7) 10.5 (-14))

-- Scenario: Multiplying a tuple by a fraction
testTuples11 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertEqual "for (multScalar a 0.5) == tuple 0.5 -1 1.5 -2)"
        (multScalar a 0.5) (tuple 0.5 (-1) 1.5 (-2))

-- Scenario: Dividing a tuple by a scalar
testTuples12 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertEqual "for (divScalar a 2) == tuple 0.5 -1 1.5 -2"
        (divScalar a 2) (tuple 0.5 (-1) 1.5 (-2))

-- Scenario: Computing the magnitude of vector(1, 0, 0)
testTuples13 = TestCase $ do
    let v = vector 1 0 0
    assertFloatsEq "for (magnitude v)" (magnitude v) 1.0

-- Scenario: Computing the magnitude of vector(0, 1, 0)
testTuples14 = TestCase $ do
    let v = vector 0 1 0
    assertFloatsEq "for (magnitude v)" (magnitude v) 1.0

-- Scenario: Computing the magnitude of vector(0, 0, 1)
testTuples15 = TestCase $ do
    let v = vector 0 0 1
    assertFloatsEq "for (magnitude v)" (magnitude v) 1.0

-- Scenario: Computing the magnitude of vector(1, 2, 3)
testTuples16 = TestCase $ do
    let v = vector 1 2 3
    assertFloatsEq "for (magnitude v)" (magnitude v) (sqrt 14)

-- Scenario: Computing the magnitude of vector(-1, -2, -3)
testTuples17 = TestCase $ do
    let v = vector (-1) (-2) (-3)
    assertFloatsEq "for (magnitude v)" (magnitude v) (sqrt 14)

-- Scenario: Normalizing vector(4, 0, 0) gives (1, 0, 0)
testTuples18 = TestCase $ do
    let v = vector 4 0 0
    assertEqual "for (magnitude v)" (normalize v) (vector 1 0 0)

-- Scenario: Normalizing vector(1, 2, 3)
testTuples19 = TestCase $ do
    let v = vector 1 2 3
    assertEqual "for (magnitude v)" (normalize v)
        (vector 0.26726 0.53452 0.80178)

-- Scenario: The magnitude of a normalized vector
testTuples20 = TestCase $ do
    let v = vector 1 2 3
    let norm = normalize v
    assertFloatsEq "for (magnitude (normalize v))" (magnitude norm) 1


-- Scenario: The dot product of two tuples
testTuples21 = TestCase $ do
    let a = vector 1 2 3
    let b = vector 2 3 4
    assertFloatsEq "for (dot a b)" (dot a b) 20

-- Scenario: The cross product of two vectors
testTuples22 = TestCase $ do
    let a = vector 1 2 3
    let b = vector 2 3 4
    assertEqual "for (cross a b)" (cross a b) (vector (-1) 2 (-1))
    assertEqual "for (cross b a)" (cross b a) (vector 1 (-2) 1)

tupleTests = TestList
    [ TestLabel "testTuples01" testTuples01
    , TestLabel "testTuples02" testTuples02
    , TestLabel "testTuples03" testTuples03
    , TestLabel "testTuples04" testTuples04
    , TestLabel "testTuples05" testTuples05
    , TestLabel "testTuples06" testTuples06
    , TestLabel "testTuples07" testTuples07
    , TestLabel "testTuples08" testTuples08
    , TestLabel "testTuples09" testTuples09
    , TestLabel "testTuples10" testTuples10
    , TestLabel "testTuples11" testTuples11
    , TestLabel "testTuples12" testTuples12
    , TestLabel "testTuples13" testTuples13
    , TestLabel "testTuples14" testTuples14
    , TestLabel "testTuples15" testTuples15
    , TestLabel "testTuples16" testTuples16
    , TestLabel "testTuples17" testTuples17
    , TestLabel "testTuples18" testTuples18
    , TestLabel "testTuples19" testTuples19
    , TestLabel "testTuples20" testTuples20
    , TestLabel "testTuples21" testTuples21
    , TestLabel "testTuples22" testTuples22
    ]

-- chapter 2 tests, canvas.feature

assertColorsEq :: String -> Color -> Color -> Assertion
assertColorsEq preface c1 c2 = unless (colorsEq c1 c2) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected: " ++ show c2 ++ "\n but got: " ++ show c1

-- Scenario: Colors are (red, green, blue) tuples
testCanvas01 = TestCase $ do
    let c = color (-0.5) 0.4 1.7
    assertEqual "for (red c)" (red c) (-0.5)
    assertEqual "for (green c)" (red c) (-0.5)
    assertEqual "for (blue c)" (red c) (-0.5)

-- Scenario: Adding colors
testCanvas02 = TestCase $ do
    let c1 = color 0.9 0.6 0.75
    let c2 = color 0.7 0.1 0.25
    assertColorsEq "for (addColors c1 c2)"
        (addColors c1 c2) (color 1.6 0.7 1.0)

-- Scenario: Subtracting colors
testCanvas03 = TestCase $ do
    let c1 = color 0.9 0.6 0.75
    let c2 = color 0.7 0.1 0.25
    assertColorsEq "for (subColors c1 c2)"
        (subColors c1 c2) (color 0.2 0.5 0.5)

-- Scenario: Multiplying a color by a scalar
testCanvas04 = TestCase $ do
    let c = color 0.2 0.3 0.4
    assertColorsEq "for (multColorScalar c 2)"
        (multColorScalar c 2) (color 0.4 0.6 0.8)

-- Scenario: Multiplying colors
testCanvas05 = TestCase $ do
    let c1 = color 1 0.2 0.4
    let c2 = color 0.9 1 0.1
    assertColorsEq "for (multColors c1 c2)"
        (multColors c1 c2) (color 0.9 0.2 0.04)

-- Scenario: Creating a canvas
testCanvas06 = TestCase $ do
    let c = canvas 10 20
    assertEqual "for (width c)" (canvasWidth c) 10
    assertEqual "for (height c)" (canvasHeight c) 20
    assertBool "for (all (== color 0 0 0) c)"
        (V.all (== color 0 0 0) (pixels c))

-- Scenario: Writing pixels to a canvas
testCanvas07 = TestCase $ do
    let c = canvas 10 20
    let red = color 1 0 0
    let c' = writePixel c 2 3 red
    assertEqual "for (pixelAt 2 3)" (pixelAt c' 2 3) red

-- Scenario: Constructing the PPM header
testCanvas08 = TestCase $ do
    let c = canvas 5 3
    let ppm = canvasToPpm c
    assertEqual "for (canvasToPpm c)"
        ["P3", "5 3", "255"] (take 3 ppm)

-- Scenario: Constructing the PPM pixel data
testCanvas09 = TestCase $ do
    let c = canvas 5 3
    let c1 = color 1.5 0 0
    let c2 = color 0 0.5 0
    let c3 = color (-0.5) 0 1
    let c' = writePixel c 0 0 c1
    let c'' = writePixel c' 2 1 c2
    let c''' = writePixel c'' 4 2 c3
    let ppm = canvasToPpm c'''
    assertEqual "for (canvasToPpm c''')"
        [ "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        , "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
        , "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
        ]
        (take 3 . drop 3 $ ppm)

-- Scenario: Splitting long lines in PPM files
testCanvas10 = TestCase $ do
    let c = canvas 10 2
    let c' = c { pixels = V.map (const $ color 1 0.8 0.6) (pixels c) }
    let ppm = canvasToPpm c'
    assertEqual "for (canvasToPpm c')"
        [ "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        , "153 255 204 153 255 204 153 255 204 153 255 204 153"
        , "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        , "153 255 204 153 255 204 153 255 204 153 255 204 153"
        ]
        (take 3 . drop 3 $ ppm)

-- Scenario: PPM files are terminated by a newline character
testCanvas11 = TestCase $ do
    let c = canvas 5 3
    let ppm = canvasToPpm c
    assertEqual "for (canvasToPpm c)"
        "\n" (head . reverse $ ppm)

canvasTests = TestList
    [ TestLabel "testCanvas01" testCanvas01
    , TestLabel "testCanvas02" testTuples02
    , TestLabel "testCanvas03" testTuples03
    , TestLabel "testCanvas04" testTuples04
    , TestLabel "testCanvas05" testTuples05
    , TestLabel "testCanvas06" testTuples06
    , TestLabel "testCanvas07" testTuples07
    , TestLabel "testCanvas08" testTuples08
    , TestLabel "testCanvas09" testTuples09
    , TestLabel "testCanvas10" testTuples10
    , TestLabel "testCanvas11" testTuples11
    ]

-- chapter 3 tests, matrices.feature

-- Scenario: Constructing and inspecting a 4x4 matrix
testMatrix01 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList
                [ 1,    2,    3,    4
                , 5.5,  6.5,  7.5,  8.5
                , 9,    10,   11,   12
                , 13.5, 14.5, 15.5, 16.5
                ]
    assertEqual "for (elemAt 0 0)" 1 (elemAt 0 0 m)
    assertEqual "for (elemAt 0 3)" 4 (elemAt 0 3 m)
    assertEqual "for (elemAt 1 0)" 5.5 (elemAt 1 0 m)
    assertEqual "for (elemAt 1 2)" 7.5 (elemAt 1 2 m)
    assertEqual "for (elemAt 2 2)" 11 (elemAt 2 2 m)
    assertEqual "for (elemAt 3 0)" 13.5 (elemAt 3 0 m)
    assertEqual "for (elemAt 3 2)" 15.5 (elemAt 3 2 m)

-- Scenario: A 2x2 matrix ought to be representable
testMatrix02 = TestCase $ do
    let m = Matrix 2 2 $ V.fromList [-3, 5, 1, -2]
    assertEqual "for (elemAt 0 0)" (-3) (elemAt 0 0 m)
    assertEqual "for (elemAt 0 1)" 5 (elemAt 0 1 m)
    assertEqual "for (elemAt 1 0)" 1 (elemAt 1 0 m)
    assertEqual "for (elemAt 1 1)" (-2) (elemAt 1 1 m)

-- Scenario: A 3x3 matrix ought to be representable
testMatrix03 = TestCase $ do
    let m = Matrix 3 3 $ V.fromList [-3, 5, 0, 1, -2, -7, 0, 1, 1]
    assertEqual "for (elemAt 0 0)" (-3) (elemAt 0 0 m)
    assertEqual "for (elemAt 1 1)" (-2) (elemAt 1 1 m)
    assertEqual "for (elemAt 2 2)" 1 (elemAt 2 2 m)

-- Scenario: Matrix equality with identical matrices
testMatrix04 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
    let n = Matrix 4 4 $ V.fromList [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
    assertEqual "for m == n" m n

-- Scenario: Matrix equality with different matrices
testMatrix05 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
    let n = Matrix 4 4 $ V.fromList [2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1]
    assertBool "for m /= n" (m /= n)

-- Scenario: Multiplying two matrices
testMatrix06 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
    let n = Matrix 4 4 $ V.fromList [-2,1,2,3,3,2,1,-1,4,3,6,5,1,2,7,8]
    let r = Matrix 4 4 $ V.fromList
                [20,22,50,48,44,54,114,108,40,58,110,102,16,26,46,42]
    assertEqual "for (matrixMult m n)" r (matrixMult m n)

-- Scenario: A matrix multiplied by a tuple
testMatrix07 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [1,2,3,4,2,4,4,2,8,6,4,1,0,0,0,1]
    let b = tuple 1 2 3 1
    assertEqual "for (matrixMult m b)" (matrixMult m b)
        (tuple 18 24 33 1)

-- Scenario: Transposing a matrix
testMatrix08 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [0,9,3,0,9,8,0,8,1,8,5,3,0,0,5,8]
    let r = Matrix 4 4 $ V.fromList [0,9,1,0,9,8,8,0,3,0,5,5,0,8,3,8]
    assertEqual "for (transpose m)" (transpose m) r

-- Scenario: Transposing the identity matrix
testMatrix09 = TestCase $ do
    assertEqual "for (tranpose identityMatrix4)"
        (transpose identityMatrix4) identityMatrix4

-- Scenario: Calculating the determinant of a 2x2 matrix
testMatrix10 = TestCase $ do
    let m = Matrix 2 2 $ V.fromList [1,5,-3,2]
    assertFloatsEq "for (determinant m)" (determinant m) 17

-- Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix
testMatrix11 = TestCase $ do
    let m = Matrix 3 3 $ V.fromList [1,5,0,-3,2,7,0,6,-3]
    let s = Matrix 2 2 $ V.fromList [-3,2,0,6]
    assertEqual "for (submatrix m 0 2)" s (submatrix m 0 2)

-- Scenario: A submatrix of a 4x4 matrix is a 3x3 matrix
testMatrix12 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [-6,1,1,6,-8,5,8,6,-1,0,8,2,-7,1,-1,1]
    let s = Matrix 3 3 $ V.fromList [-6,1,6,-8,8,6,-7,-1,1]
    assertEqual "for (submatrix m 2 1)" s (submatrix m 2 1)

-- Scenario: Calculating a minor of a 3x3 matrix
testMatrix13 = TestCase $ do
    let m = Matrix 3 3 $ V.fromList [3,5,0,2,-1,-7,6,-1,5]
    let b = submatrix m 1 0
    assertFloatsEq "for (determinant b)" 25 (determinant b)
    assertFloatsEq "for (minor m 1 0)" 25 (minor m 1 0)

-- Scenario: Calculating a cofactor of a 3x3 matrix
testMatrix14 = TestCase $ do
    let m = Matrix 3 3 $ V.fromList [3,5,0,2,-1,-7,6,-1,5]
    assertFloatsEq "for (minor m 0 0)" (-12) (minor m 0 0)
    assertFloatsEq "for (cofactor m 0 0)" (-12) (cofactor m 0 0)
    assertFloatsEq "for (minor m 1 0)" 25 (minor m 1 0)
    assertFloatsEq "for (cofactor m 1 0)" (-25) (cofactor m 1 0)

-- Scenario: Calculating the determinant of a 3x3 matrix
testMatrix15 = TestCase $ do
    let m = Matrix 3 3 $ V.fromList [1,2,6,-5,8,-4,2,6,4]
    assertFloatsEq "for (cofactor m 0 0)" 56 (cofactor m 0 0)
    assertFloatsEq "for (cofactor m 0 1)" 12 (cofactor m 0 1)
    assertFloatsEq "for (cofactor m 0 2)" (-46) (cofactor m 0 2)
    assertFloatsEq "for (determinant m)" (-196) (determinant m)

-- Scenario: Calculating the determinant of a 4x4 matrix
testMatrix16 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [-2,-8,3,5,-3,1,7,3,1,2,-9,6,-6,7,7,-9]
    assertFloatsEq "for (cofactor m 0 0)" 690 (cofactor m 0 0)
    assertFloatsEq "for (cofactor m 0 1)" 447 (cofactor m 0 1)
    assertFloatsEq "for (cofactor m 0 2)" 210 (cofactor m 0 2)
    assertFloatsEq "for (cofactor m 0 3)" 51 (cofactor m 0 3)
    assertFloatsEq "for (determinant m)" (-4071) (determinant m)

-- Scenario: Testing an invertible matrix for invertibility
testMatrix17 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [6,4,4,4,5,5,7,6,4,-9,3,-7,9,1,7,-6]
    assertFloatsEq "for (determinant m)" (-2120) (determinant m)
    assertBool "for (invertable m)" (invertable m)

-- Scenario: Testing a noninvertible matrix for invertibility
testMatrix18 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [-4,2,-2,-3,9,6,2,6,0,-5,1,-5,0,0,0,0]
    assertFloatsEq "for (determinant m)" 0 (determinant m)
    assertBool "for (not $ invertable m)" (not $ invertable m)

-- Scenario: Calculating the inverse of a matrix
testMatrix19 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList [-5,2,6,-8,1,-5,1,8,7,7,-6,-7,1,-3,7,4]
    let b = inverse m
    assertFloatsEq "for (determinant m)" 532 (determinant m)
    assertFloatsEq "for (cofactor m 2 3)" (-160) (cofactor m 2 3)
    assertFloatsEq "for (elemAt 3 2 b)" (-160/532) (elemAt 3 2 b)
    assertFloatsEq "for (cofactor m 3 2)" 105 (cofactor m 3 2)
    assertFloatsEq "for (elemAt 2 3 b)" (105/532) (elemAt 2 3 b)
    assertEqual "for (inverse m)"
        (Matrix 4 4 $ V.fromList
            [ 0.21805,   0.45113,  0.24060, -0.04511
            , -0.80827, -1.45677, -0.44361,  0.52068
            , -0.07895, -0.22368, -0.05263,  0.19737
            , -0.52256, -0.81391, -0.30075,  0.30639
            ]
        ) b

-- Scenario: Calculating the inverse of another matrix
testMatrix20 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList
                [  8, -5,  9,  2
                ,  7,  5,  6,  1
                , -6,  0,  9,  6
                , -3,  0, -9, -4
                ]
    let n = Matrix 4 4 $ V.fromList
                [ -0.15385, -0.15385, -0.28205, -0.53846
                , -0.07692,  0.12308,  0.02564,  0.03077
                ,  0.35897,  0.35897,  0.43590,  0.92308
                , -0.69231, -0.69231, -0.76923, -1.92308
                ]
    assertEqual "for (inverse m)" n (inverse m)

-- Scenario: Calculating the inverse of a third matrix
testMatrix21 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList
                [  9,  3,  0,  9
                , -5, -2, -6, -3
                , -4,  9,  6,  4
                , -7,  6,  6,  2
                ]
    let n = Matrix 4 4 $ V.fromList
                [ -0.04074, -0.07778,  0.14444, -0.22222
                , -0.07778,  0.03333,  0.36667, -0.33333
                , -0.02901, -0.14630, -0.10926,  0.12963
                ,  0.17778,  0.06667, -0.26667,  0.33333
                ]
    assertEqual "for (inverse m)" n (inverse m)

-- Scenario: Multiplying a product by its inverse
testMatrix22 = TestCase $ do
    let m = Matrix 4 4 $ V.fromList
                [  3, -9,  7,  3
                ,  3, -8,  2, -9
                , -4,  4,  4,  1
                , -6,  5, -1,  1
                ]
    let n = Matrix 4 4 $ V.fromList
                [ 8,  2, 2, 2
                , 3, -1, 7, 0
                , 7,  0, 5, 4
                , 6, -2, 0, 5
                ]
    assertEqual "for (matrixMult (matrixMult m n) (inverse n))"
        m
        (matrixMult (matrixMult m n) (inverse n))

matrixTests = TestList
    [ TestLabel "testMatrix01" testMatrix01
    , TestLabel "testMatrix02" testMatrix02
    , TestLabel "testMatrix03" testMatrix03
    , TestLabel "testMatrix04" testMatrix04
    , TestLabel "testMatrix05" testMatrix05
    , TestLabel "testMatrix06" testMatrix06
    , TestLabel "testMatrix07" testMatrix07
    , TestLabel "testMatrix08" testMatrix08
    , TestLabel "testMatrix09" testMatrix09
    , TestLabel "testMatrix10" testMatrix10
    , TestLabel "testMatrix11" testMatrix11
    , TestLabel "testMatrix12" testMatrix12
    , TestLabel "testMatrix13" testMatrix13
    , TestLabel "testMatrix14" testMatrix14
    , TestLabel "testMatrix15" testMatrix15
    , TestLabel "testMatrix16" testMatrix16
    , TestLabel "testMatrix17" testMatrix17
    , TestLabel "testMatrix18" testMatrix18
    , TestLabel "testMatrix19" testMatrix19
    , TestLabel "testMatrix20" testMatrix20
    , TestLabel "testMatrix21" testMatrix21
    , TestLabel "testMatrix22" testMatrix22
    ]

-- chapter 4 tests, transformations.feature

-- Scenario: Multiplying by a translation matrix
testTransform01 = TestCase $ do
    let t = translation 5 (-3) 2
    let p = point (-3) 4 5
    assertEqual "for (matrixMult t p)" (point 2 1 7) (matrixMult t p)

-- Scenario: Multiplying by the inverse of a translation matrix
testTransform02 = TestCase $ do
    let t = translation 5 (-3) 2
    let inv = inverse t
    let p = point (-3) 4 5
    assertEqual "for (matrixMult inv p)" (point (-8) 7 3) (matrixMult inv p)

-- Scenario: Translation does not affect vectors
testTransform03 = TestCase $ do
    let t = translation 5 (-3) 2
    let v = vector (-3) 4 5
    assertEqual "for (matrixMult t v)" v (matrixMult t v)

-- Scenario: A scaling matrix applied to a point
testTransform04 = TestCase $ do
    let t = scaling 2 3 4
    let p = point (-4) 6 8
    assertEqual "for (matrixMult t p)" (point (-8) 18 32) (matrixMult t p)

-- Scenario: A scaling matrix applied to a vector
testTransform05 = TestCase $ do
    let t = scaling 2 3 4
    let v = vector (-4) 6 8
    assertEqual "for (matrixMult t v)" (vector (-8) 18 32) (matrixMult t v)

-- Scenario: Multiplying by the inverse of a scaling matrix
testTransform06 = TestCase $ do
    let t = scaling 2 3 4
    let inv = inverse t
    let v = vector (-4) 6 8
    assertEqual "for (matrixMult inv v)" (vector (-2) 2 2) (matrixMult inv v)

-- Scenario: Reflection is scaling by a negative value
testTransform07 = TestCase $ do
    let t = scaling (-1) 1 1
    let p = point 2 3 4
    assertEqual "for (matrixMult t p)" (point (-2) 3 4) (matrixMult t p)

-- Scenario: Rotating a point around the x axis
testTransform08 = TestCase $ do
    let p = point 0 1 0
    let halfQuarter = rotationX (pi / 4)
    let fullQuarter = rotationX (pi / 2)
    assertEqual "for (matrixMult halfQuarter p)"
        (point 0 (sqrt 2 / 2) (sqrt 2 /2)) (matrixMult halfQuarter p)
    assertEqual "for (matrixMult fullQuarter p)"
        (point 0 0 1) (matrixMult fullQuarter p)

-- Scenario: The inverse of an x-rotation rotates in the opposite direction
testTransform09 = TestCase $ do
    let p = point 0 1 0
    let halfQuarter = rotationX (pi / 4)
    let inv = inverse halfQuarter
    assertEqual "for (matrixMult inv p)"
        (point 0 (sqrt 2 / 2) (-(sqrt 2) / 2)) (matrixMult inv p)

-- Scenario: Rotating a point around the y axis
testTransform10 = TestCase $ do
    let p = point 0 0 1
    let halfQuarter = rotationY (pi / 4)
    let fullQuarter = rotationY (pi / 2)
    assertEqual "for (halfQurter % p)"
        (point (sqrt 2 / 2) 0 (sqrt 2 / 2)) (halfQuarter % p)
    assertEqual "for (fullQuarter % p)"
        (point 1 0 0) (fullQuarter % p)

-- Scenario: Rotating a point around the z axis
testTransform11 = TestCase $ do
    let p = point 0 1 0
    let halfQuarter = rotationZ (pi / 4)
    let fullQuarter = rotationZ (pi / 2)
    assertEqual "for (halfQuarter % p)"
        (point (-(sqrt 2) / 2) (sqrt 2 / 2) 0) (halfQuarter % p)
    assertEqual "for (fullQuarter % p)"
        (point (-1) 0 0) (fullQuarter % p)

-- Scenario: A shearing transformation moves x in proportion to y
testTransform12 = TestCase $ do
    let t = shearing 1 0 0 0 0 0
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 5 3 4) (t % p)

-- Scenario: A shearing transformation moves x in proportion to z
testTransform13 = TestCase $ do
    let t = shearing 0 1 0 0 0 0
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 6 3 4) (t % p)

-- Scenario: A shearing transformation moves y in proportion to x
testTransform14 = TestCase $ do
    let t = shearing 0 0 1 0 0 0
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 2 5 4) (t % p)

-- Scenario: A shearing transformation moves y in proportion to z
testTransform15 = TestCase $ do
    let t = shearing 0 0 0 1 0 0
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 2 7 4) (t % p)

-- Scenario: A shearing transformation moves z in proportion to x
testTransform16 = TestCase $ do
    let t = shearing 0 0 0 0 1 0
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 2 3 6) (t % p)

-- Scenario: A shearing transformation moves z in proportion to y
testTransform17 = TestCase $ do
    let t = shearing 0 0 0 0 0 1
    let p = point 2 3 4
    assertEqual "for (t % p)" (point 2 3 7) (t % p)

-- Scenario: Individual transformations are applied in sequence
testTransform18 = TestCase $ do
    let p = point 1 0 1
    let a = rotationX (pi / 2)
    let b = scaling 5 5 5
    let c = translation 10 5 7
    let p2 = a % p
    let p3 = b % p2
    let p4 = c % p3
    assertEqual "for (a % p)" (point 1 (-1) 0) p2
    assertEqual "for (b % p2)" (point 5 (-5) 0) p3
    assertEqual "for (c % p3)" (point 15 0 7) p4

-- Scenario: Chained transformations must be applied in reverse order
testTransform19 = TestCase $ do
    let p = point 1 0 1
    let a = rotationX (pi / 2)
    let b = scaling 5 5 5
    let c = translation 10 5 7
    let t = c % b % a
    assertEqual "for (t % p)" (point 15 0 7) (t % p)

transformTests = TestList
    [ TestLabel "testTransform01" testTransform01
    , TestLabel "testTransform02" testTransform02
    , TestLabel "testTransform03" testTransform03
    , TestLabel "testTransform04" testTransform04
    , TestLabel "testTransform05" testTransform05
    , TestLabel "testTransform06" testTransform06
    , TestLabel "testTransform07" testTransform07
    , TestLabel "testTransform08" testTransform08
    , TestLabel "testTransform09" testTransform09
    , TestLabel "testTransform10" testTransform10
    , TestLabel "testTransform11" testTransform11
    , TestLabel "testTransform12" testTransform12
    , TestLabel "testTransform13" testTransform13
    , TestLabel "testTransform14" testTransform14
    , TestLabel "testTransform15" testTransform15
    , TestLabel "testTransform16" testTransform16
    , TestLabel "testTransform17" testTransform17
    , TestLabel "testTransform18" testTransform18
    , TestLabel "testTransform19" testTransform19
    ]

main :: IO ()
main = do
    results <- runTestTT $ TestList
        [tupleTests, canvasTests, matrixTests, transformTests]
    if (errors results + failures results == 0)
        then exitWith ExitSuccess
        else exitWith (ExitFailure 1)
