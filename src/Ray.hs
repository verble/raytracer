
module Ray where

import Matrix


data Ray = Ray
    { rayOrigin :: Matrix
    , direction :: Matrix
    } deriving (Show, Eq)

ray :: Matrix -> Matrix -> Ray
ray origin direction
    | isPoint origin && isVector direction = Ray origin direction
    | otherwise = error "invalid origin or direction"


position :: Ray -> Double -> Matrix
position (Ray origin direction) time =
    matrixAdd (multScalar direction time) origin

data Sphere = Sphere
    { sphereOrigin :: Matrix
    , radius :: Double
    , sphereTransform :: Matrix
    } deriving (Show)

instance Eq Sphere where
    (==) (Sphere originA radiusA transformA) (Sphere originB radiusB transformB)
        = originA == originB
            && floatsEq radiusA radiusB
            && transformA == transformB

sphere :: Sphere
sphere = Sphere (point 0 0 0) 1 identityMatrix4

setTransform :: Sphere -> Matrix -> Sphere
setTransform (Sphere orig r t) m@(Matrix numCols numRows _)
    | numCols /= 4 || numRows /= 4 = error "invalid transform matrix"
    | otherwise = Sphere orig r m

intersects :: Sphere -> Ray -> Intersections
intersects s@(Sphere spOrig radius sphTrans) ray
    | discriminant < 0 = []
    | otherwise = map (`intersection` s) [t1, t2]
        -- TODO: need to ensure increasing order?
    where
        -- transform the ray before doing calculations
        (Ray rayOrigin direction) = transform ray (inverse sphTrans)
        -- vector from sphere's center to ray's origin
        sphereToRay = matrixSub rayOrigin (point 0 0 0)
        a = dot direction direction
        b = 2 * dot direction sphereToRay
        c = dot sphereToRay sphereToRay - 1
        -- if negative, no intersections
        discriminant = (b ** 2) - 4 * a * c
        -- else, intersections are
        t1 = ((-b) - sqrt discriminant) / (2 * a)
        t2 = ((-b) + sqrt discriminant) / (2 * a)

data Intersection = Intersection
    { tValue :: Double
    , object :: Sphere
    } deriving (Show)

instance Eq Intersection where
    (==) (Intersection t1 o1) (Intersection t2 o2)
        = floatsEq t1 t2 && o1 == o2

intersection :: Double -> Sphere -> Intersection
intersection = Intersection

type Intersections = [Intersection]

intersections :: [Intersection] -> Intersections
intersections = id

hit :: Intersections -> Maybe Intersection
hit = foldl reducer Nothing
    -- the hit is always the lowest nonnegative intersection
    where reducer current@(Just (Intersection t1 _)) next@(Intersection t2 _)
              | t2 < 0    = current
              | t2 < t1   = Just next
              | otherwise = current
          reducer Nothing next@(Intersection t2 _)
              | t2 < 0    = Nothing
              | otherwise = Just next

transform :: Ray -> Matrix -> Ray
transform (Ray rayOrigin direction) transform
    = Ray (transform % rayOrigin) (transform % direction)
