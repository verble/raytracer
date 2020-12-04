
module Canvas where

import qualified Data.Vector.Unboxed as V
import Data.List (intercalate)

import Matrix

type Color = (Double, Double, Double)

color :: Double -> Double -> Double -> Color
color r g b = (r, g, b)

red :: Color -> Double
red (r, _, _) = r

green :: Color -> Double
green (_, g, _) = g

blue :: Color -> Double
blue (_, _, b) = b

colorsEq :: Color -> Color -> Bool
colorsEq (r1, g1, b1) (r2, g2, b2) =
    floatsEq r1 r2 && floatsEq g1 g2 && floatsEq b1 b2

addColors :: Color -> Color -> Color
addColors (r1, g1, b1) (r2, g2, b2) =
    (r1 + r2, g1 + g2, b1 + b2)

subColors :: Color -> Color -> Color
subColors (r1, g1, b1) (r2, g2, b2) =
    (r1 - r2, g1 - g2, b1 - b2)

multColorScalar :: Color -> Double -> Color
multColorScalar (r, g, b) s = (r * s, g * s, b * s)

multColors :: Color -> Color -> Color
multColors (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

data Canvas = Canvas
    { canvasWidth  :: Int
    , canvasHeight :: Int
    , pixels       :: V.Vector Color
    } deriving (Eq, Show)

canvas :: Int -> Int -> Canvas
canvas width height = Canvas width height $
    V.replicate (width * height) (color 0 0 0)

writePixel :: Canvas -> Int -> Int -> Color -> Canvas
writePixel (Canvas w h pixels) x y pixel
    | x < 0     = error msg
    | y < 0     = error msg
    | x >= w    = error msg
    | y >= h    = error msg
    | otherwise = Canvas w h (pixels V.// [(ix, pixel)])
    where msg = "index out of bounds: (" ++ show x ++ ", " ++ show y ++ ")"
          ix = (w * y) + x

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt c x y = pixels c V.! ix
    where ix = (canvasWidth c * y) + x

type PPM = [String]

writePpm :: PPM -> String -> IO ()
writePpm ppm filename = do
    writeFile filename (unlines ppm ++ "\n")

canvasToPpm :: Canvas -> PPM
canvasToPpm c@(Canvas w h pixels) = ppmHeader ++ rows
    where ppmHeader = ["P3", show w ++ " " ++ show h, "255"]
          rows = toLines c

toLines (Canvas w h pixels) =
    concatMap (lines . serializeRow 0) . groupsOf w
        . normalize256 . concatMap uncolor . V.toList
        $ pixels

uncolor :: Color -> [Double]
uncolor (r, g, b) = [r, g, b]

normalize256 :: [Double] -> [Int]
normalize256 = map (round . clamp . normalize)
    where normalize n = n * 255
          clamp n
              | n < 0     = 0
              | n > 255   = 255
              | otherwise = n

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

numWidth :: Int -> Int
numWidth 0 = 1
numWidth x = floor (logBase 10 (fromIntegral $ abs x)) + 1

serializeRow :: Int -> [Int] -> String
serializeRow _ [] = ""
serializeRow 0 (x:xs) = show x ++ serializeRow (numWidth x) xs
serializeRow n (x:xs)
    | nextColPos > ppmLineLimit = "\n" ++ serializeRow 0 (x:xs)
    | otherwise = " " ++ show x ++ serializeRow nextColPos xs
    where nextColPos = numWidth x + n + 1
          ppmLineLimit = 70
