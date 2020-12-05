
module Main where

import Data.List (foldl')

import Canvas
import Matrix
import Ray

main = writeCanvas image "rendered.ppm"


-- light located at xy origin and forward 10 units on z axis
light = point 0 0 (-5)

-- sphere is unit sphere at origin
obj1 = setTransform sphere (scaling 1 0.5 1)

-- need a ray for every pixel on canvas
rays = do
    -- canvas is 300 x 300 pixels
    -- canvas located at (point 0 0 -2) and is 6x6 units
    let canvasZ = 10
    let xPxCoords = [0..99] :: [Int]
    let yPxCoords = [0..99] :: [Int]
    let unitsPerPixel = 7 / 100
    let convertX x = (fromIntegral x - 50) * unitsPerPixel
    let convertY y = (fromIntegral y - 50) * unitsPerPixel
    x <- xPxCoords
    y <- yPxCoords
    let canvasCoords = (x,y)
    let worldCoords@(worldX, worldY) = (convertX x, convertY y)
    let dir = matrixSub (point worldX worldY canvasZ) light
    pure ((x,y), ray light dir)

pixelData = map helper rays
    where helper (canvasCoords, ray) =
              case hit (intersects obj1 ray) of
                  Nothing -> (canvasCoords, black)
                  Just _ -> (canvasCoords, color 1 0 0)

image = foldl'
    (\canvas ((x,y),col) -> writePixel canvas x (100-y-1) col)
    (canvas 100 100)
    (pixelData)
