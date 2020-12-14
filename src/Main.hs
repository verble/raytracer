
module Main where

import Data.List (foldl')
import Control.Monad
import System.ProgressBar
import Control.DeepSeq

import Canvas
import Matrix
import Ray

main' = writeCanvas image "rendered.ppm"

-- purply color
mat1 = material { materialColor = color 1 0.2 1 }
-- sphere is unit sphere at origin
obj1 = sphere { sphereMaterial = mat1 }
obj2 = (setTransform sphere (scaling 1 0.5 1)) { sphereMaterial = mat1 }

-- light located at xy origin and forward 10 units on z axis
light1 = pointLight (point (-10) (-10) (-10)) (color 1 1 1)

-- pixels on screen
canvasSize = 300

screenCoords :: [((Int, Int), Matrix)]
screenCoords = do
    -- points in worldspace
    let screenSize = 7
    -- screen is located at x=0 and y=0
    let screenToWorld = (* (screenSize / fromIntegral canvasSize))
            . subtract (fromIntegral canvasSize / 2)
            . fromIntegral
    x <- [0..canvasSize-1]
    y <- [0..canvasSize-1]
    pure $ ((x, y), point (screenToWorld x) (screenToWorld y) 10)

castOrigin :: Matrix
castOrigin = point 0 0 (-5)

-- calculates the ray from the "eye" to the screen
pixelToRay :: Matrix -> Ray
pixelToRay point = Ray castOrigin (normalize $ matrixSub point castOrigin)

renderPixel :: Matrix -> Color
renderPixel screenPos =
    case hit (intersects obj1 r) of
        Nothing -> black
        Just h@(Intersection tValue object) ->
            let point = position r tValue
                normal = normalAt object point
                eye = negTuple (direction r)
            in lighting (sphereMaterial obj1) light1 point eye normal
    where r = pixelToRay screenPos

pixelData = map helper screenCoords
    where helper (canvasCoords, ray) = (canvasCoords, renderPixel ray)

image = foldl'
    (\canvas ((x,y),col) -> writePixel canvas x y col)
    (canvas 100 100)
    (pixelData)

-- helper' :: ProgressBar s -> Canvas -> ((Int,Int), Matrix) -> IO Canvas
helper' pb canvas ((x,y), ray) = do
    let color = renderPixel ray
    deepseq color $ incProgress pb 1
    pure (writePixel canvas x y color)

main = do
    pb <- newProgressBar
        defStyle 10 (Progress 0 (canvasSize * canvasSize) ())
    c <- foldM (helper' pb) (canvas canvasSize canvasSize) screenCoords
    putStrLn "done!"
    writeCanvas c "rendered.ppm"
