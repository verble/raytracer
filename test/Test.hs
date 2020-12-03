
module Test where

import Control.Monad
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import System.Exit

import Tuple
import Canvas

assertFloatsEq :: String -> Double -> Double -> Assertion
assertFloatsEq preface d1 d2 = unless (floatsEq d1 d2) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected: " ++ show d2 ++ "\n but got: " ++ show d1

assertTuplesEq :: String -> Tuple -> Tuple -> Assertion
assertTuplesEq preface t1 t2 = unless (tuplesEq t1 t2) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected: " ++ show t2 ++ "\n but got: " ++ show t1

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
    assertTuplesEq "for (point 4 (-4) 3)" p (tuple 4 (-4) 3 1)

-- Scenario: vector() creates tuples with w=0
testTuples04 = TestCase $ do
    let v = vector 4 (-4) 3
    assertTuplesEq "for (vector 4 (-4) 3)" v (tuple 4 (-4) 3 0)

-- Scenario: Adding two tuples
testTuples05 = TestCase $ do
    let a1 = tuple 3 (-2) 5 1
    let a2 = tuple (-2) 3 1 0
    assertTuplesEq "for ((addTuples a1 a2) == tuple 1 1 6 1)"
        (addTuples a1 a2) (tuple 1 1 6 1)

-- Scenario: Subtracting two points
testTuples06 = TestCase $ do
    let p1 = point 3 2 1
    let p2 = point 5 6 7
    assertTuplesEq "for ((subTuples p1 p2) == vector -2 -4 -6)"
        (subTuples p1 p2) (vector (-2) (-4) (-6))

-- Scenario: Subtracting a vector from a point
testTuples07 = TestCase $ do
    let p = point 3 2 1
    let v = vector 5 6 7
    assertTuplesEq "for (subTuples p v) == point -2 -4 -6"
        (subTuples p v) (point (-2) (-4) (-6))

-- Scenario: Subtracting a vector from the zero vector
testTuples08 = TestCase $ do
    let v = vector 1 (-2) 3
    assertTuplesEq "for (subTuples z v) == vector -1 2 -3"
        (subTuples zero v) (vector (-1) 2 (-3))

-- Scenario: Negating a tuple
testTuples09 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertTuplesEq "for (negTuple a) == tuple -1 2 -3 4"
        (negTuple a) (tuple (-1) 2 (-3) 4)

-- Scenario: Multiplying a tuple by a scalar
testTuples10 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertTuplesEq "for (multScalar a 3.5) == tuple 3.5 -7 10.5 -14)"
        (multScalar a 3.5) (tuple 3.5 (-7) 10.5 (-14))

-- Scenario: Multiplying a tuple by a fraction
testTuples11 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertTuplesEq "for (multScalar a 0.5) == tuple 0.5 -1 1.5 -2)"
        (multScalar a 0.5) (tuple 0.5 (-1) 1.5 (-2))

-- Scenario: Dividing a tuple by a scalar
testTuples12 = TestCase $ do
    let a = tuple 1 (-2) 3 (-4)
    assertTuplesEq "for (divScalar a 2) == tuple 0.5 -1 1.5 -2"
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
    assertTuplesEq "for (magnitude v)" (normalize v) (vector 1 0 0)

-- Scenario: Normalizing vector(1, 2, 3)
testTuples19 = TestCase $ do
    let v = vector 1 2 3
    assertTuplesEq "for (magnitude v)" (normalize v)
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
    assertTuplesEq "for (cross a b)" (cross a b) (vector (-1) 2 (-1))
    assertTuplesEq "for (cross b a)" (cross b a) (vector 1 (-2) 1)

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

main :: IO ()
main = do
  results <- runTestTT $ TestList [tupleTests, canvasTests]
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
