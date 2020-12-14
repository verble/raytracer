
module Ray where

import Matrix
import Canvas

import Data.Vector.Unboxed ((//))


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
    , sphereMaterial :: Material
    } deriving (Show)

instance Eq Sphere where
    (==) (Sphere originA radiusA transformA sphMatA)
        (Sphere originB radiusB transformB sphMatB)
        = originA == originB
            && floatsEq radiusA radiusB
            && transformA == transformB
            && sphMatA == sphMatB

sphere :: Sphere
sphere = Sphere (point 0 0 0) 1 identityMatrix4 material

setTransform :: Sphere -> Matrix -> Sphere
setTransform (Sphere orig r t mat) m@(Matrix numCols numRows _)
    | numCols /= 4 || numRows /= 4 = error "invalid transform matrix"
    | otherwise = Sphere orig r m mat

intersects :: Sphere -> Ray -> Intersections
intersects s@(Sphere spOrig radius sphTrans _) ray
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

normalAt :: Sphere -> Matrix -> Matrix
normalAt (Sphere o r t _) pt = normalize normInWorldSpace'
    where ptInObjSpace = (inverse t) % pt
          normInObjSpace = matrixSub ptInObjSpace (point 0 0 0)
          normInWorldSpace = transpose (inverse t) % normInObjSpace
          -- fix w coordinate by setting to 0
          normInWorldSpace' = normInWorldSpace
              { matrixData = matrixData normInWorldSpace // [(3, 0)] }

reflect :: Matrix -> Matrix -> Matrix
reflect incident normal =
    matrixSub incident (multScalar normal (2 * dot incident normal))

data PointLight = PointLight
    { lightPos :: Matrix
    , intensity :: Color
    } deriving (Show)

pointLight :: Matrix -> Color -> PointLight
pointLight = PointLight

data Material = Material
    { materialColor :: Color
    , ambient :: Double
    , diffuse :: Double
    , specular :: Double
    , shininess :: Double
    } deriving (Show)

instance Eq Material where
    (==) (Material colA ambA diffA specA shinA)
        (Material colB ambB diffB specB shinB)
        = colorsEq colA colB
            && floatsEq ambA ambB
            && floatsEq diffA diffB
            && floatsEq specA specB
            && floatsEq shinA shinB

-- the default material
material :: Material
material = Material (1, 1, 1) 0.1 0.9 0.9 200.0

lighting :: Material -> PointLight -> Matrix -> Matrix -> Matrix -> Color
lighting
    (Material materialColor ambient diffuse specular shininess)
    (PointLight lightPos intensity)
    point
    eyeVec
    normalVec
    = shading
    where
        -- combine the surface color with the light's color/intensity
        effectiveColor = multColors materialColor intensity
        -- find the direction to the light source
        lightVec = normalize (matrixSub lightPos point)
        -- compute the ambient contribution
        ambient' = multColorScalar effectiveColor ambient
        -- light_dot_normal represents the cosine of the angle between the
        -- light vector and the normal vector. A negative number means the
        -- light is on the other side of the surface.
        lightDotNormal = dot lightVec normalVec
        -- compute the diffuse contribution
        diffuse' = if lightDotNormal < 0
            then black
            else multColorScalar effectiveColor (diffuse * lightDotNormal)
        -- compute the specular contribution
        specular' = if lightDotNormal < 0
            then black
            else let
                reflectVec = reflect (negTuple lightVec) normalVec
                -- reflect_dot_eye represents the cosine of the angle between the
                -- reflection vector and the eye vector. A negative number means
                -- the light reflects away from the eye.
                reflectDotEye = dot reflectVec eyeVec
            in if reflectDotEye <= 0
                then black
                else let
                    factor = reflectDotEye ** shininess
                in multColorScalar intensity (specular * factor)
        -- Add the three contributions together to get the final shading
        shading = addColors (addColors ambient' diffuse') specular'
