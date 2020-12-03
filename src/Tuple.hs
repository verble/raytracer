module Tuple where

type Tuple = (Double, Double, Double, Double)

tuple :: Double -> Double -> Double -> Double -> Tuple
tuple x y z w = (x, y, z, w)

epsilon :: Double
epsilon = 0.00001

floatsEq :: Double -> Double -> Bool
floatsEq a b = abs (a - b) < epsilon

tuplesEq :: Tuple -> Tuple -> Bool
tuplesEq (x1, y1, z1, w1) (x2, y2, z2, w2) =
    floatsEq x1 x2 && floatsEq y1 y2 && floatsEq z1 z2 && floatsEq w1 w2

x :: Tuple -> Double
x (x, _, _, _) = x

y :: Tuple -> Double
y (_, y, _, _) = y

z :: Tuple -> Double
z (_, _, z, _) = z

w :: Tuple -> Double
w (_, _, _, w) = w

isPoint :: Tuple -> Bool
isPoint = floatsEq 1.0 . w

isVector :: Tuple -> Bool
isVector = not . floatsEq 1.0 . w

point :: Double -> Double -> Double -> Tuple
point x y z = tuple x y z 1.0

vector :: Double -> Double -> Double -> Tuple
vector x y z = tuple x y z 0.0

addTuples :: Tuple -> Tuple -> Tuple
addTuples (x1, y1, z1, w1) (x2, y2, z2, w2) =
    (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

subTuples :: Tuple -> Tuple -> Tuple
subTuples (x1, y1, z1, w1) (x2, y2, z2, w2) =
    (x1 - x2, y1 - y2, z1 - z2, w1 - w2)

zero :: Tuple
zero = vector 0 0 0

negTuple :: Tuple -> Tuple
negTuple (x, y, z, w) = (-x, -y, -z, -w)

multScalar :: Tuple -> Double -> Tuple
multScalar (x, y, z, w) s = (x * s, y * s, z * s, w * s)

divScalar :: Tuple -> Double -> Tuple
divScalar (x, y, z, w) s = (x / s, y / s, z / s, w / s)

magnitude :: Tuple -> Double
magnitude (x, y, z, w) = sqrt ((x ** 2) + (y ** 2) + (z ** 2) + (w ** 2))

normalize :: Tuple -> Tuple
normalize t@(x, y, z, w) = (x / m, y / m, z / m, w / m)
    where m = magnitude t

dot :: Tuple -> Tuple -> Double
dot (x1, y1, z1, w1) (x2, y2, z2, w2) =
    (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)

cross :: Tuple -> Tuple -> Tuple
cross (x1, y1, z1, w1) (x2, y2, z2, w2) =
    vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
