
module Main where

import Canvas
import Matrix

main = writeCanvas diagram "rendered.ppm"

origin = point 0 0 0
radius = translation 0 50 0
tick = rotationZ (-(2 * pi / 12))

-- represents a point at the 12 o'clock position
noon = radius % origin

points = map (\m -> (round $ x m, round $ y m)) . take 12 $ iterate (tick %) noon

plot :: Canvas -> (Int, Int) -> Canvas
plot canvas (x,y) = writePixel canvas (55 + x) (55 + y) white

diagram = foldl plot (canvas 110 110) points
