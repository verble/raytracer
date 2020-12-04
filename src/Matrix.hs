
module Matrix where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Control.Monad
import Data.STRef

data Matrix = Matrix
    { numRows :: !Int
    , numCols :: !Int
    , matrixData :: !(V.Vector Double)
    } deriving (Show)

instance Eq Matrix where
    (==) (Matrix numRowsA numColsA dataA) (Matrix numRowsB numColsB dataB) =
        (numRowsA == numRowsB)
            && (numRowsB == numRowsA)
            && (V.and $ V.zipWith (\a b -> floatsEq a b) dataA dataB)

tuple :: Double -> Double -> Double -> Double -> Matrix
tuple x y z w = Matrix 4 1 $ V.fromList [x, y, z, w]

epsilon :: Double
epsilon = 0.00001

floatsEq :: Double -> Double -> Bool
floatsEq a b = abs (a - b) < epsilon

x :: Matrix -> Double
x (Matrix numRows numCols matrixData)
    | numRows /= 4 || numCols /= 1 = error "matrix is not a tuple"
    | otherwise = matrixData V.! 0

y :: Matrix -> Double
y (Matrix numRows numCols matrixData)
    | numRows /= 4 || numCols /= 1 = error "matrix is not a tuple"
    | otherwise = matrixData V.! 1

z :: Matrix -> Double
z (Matrix numRows numCols matrixData)
    | numRows /= 4 || numCols /= 1 = error "matrix is not a tuple"
    | otherwise = matrixData V.! 2

w :: Matrix -> Double
w (Matrix numRows numCols matrixData)
    | numRows /= 4 || numCols /= 1 = error "matrix is not a tuple"
    | otherwise = matrixData V.! 3

isPoint :: Matrix -> Bool
isPoint = floatsEq 1.0 . w

isVector :: Matrix -> Bool
isVector = not . floatsEq 1.0 . w

point :: Double -> Double -> Double -> Matrix
point x y z = tuple x y z 1.0

vector :: Double -> Double -> Double -> Matrix
vector x y z = tuple x y z 0.0

addTuples :: Matrix -> Matrix -> Matrix
addTuples = matrixAdd

subTuples :: Matrix -> Matrix -> Matrix
subTuples = matrixSub

zero4x1 :: Matrix
zero4x1 = vector 0 0 0

negTuple :: Matrix -> Matrix
negTuple (Matrix numRows numCols matrixData) =
    Matrix numRows numCols $ V.map negate matrixData

multScalar :: Matrix -> Double -> Matrix
multScalar (Matrix numRows numCols matrixData) scalar =
    Matrix numRows numCols $ V.map (* scalar) matrixData

divScalar :: Matrix -> Double -> Matrix
divScalar (Matrix numRows numCols matrixData) scalar =
    Matrix numRows numCols $ V.map (/ scalar) matrixData

magnitude :: Matrix -> Double
magnitude m@(Matrix numRows numCols _)
    | numRows /= 4 || numCols /= 1 = error "matrix is not a tuple"
    | otherwise = sqrt ((x m ** 2) + (y m ** 2) + (z m ** 2) + (w m ** 2))

normalize :: Matrix -> Matrix
normalize matrix = divScalar matrix (magnitude matrix)

dot :: Matrix -> Matrix -> Double
dot m@(Matrix numRowsA numColsA _) n@(Matrix numRowsB numColsB _)
    | numRowsA /= 4 || numColsA /= 1 = error "matrix is not a tuple"
    | numRowsB /= 4 || numColsB /= 1 = error "matrix is not a tuple"
    | otherwise = (x m * x n) + (y m * y n) + (z m * z n) + (w m * w n)

cross :: Matrix -> Matrix -> Matrix
cross m@(Matrix numRowsA numColsA _) n@(Matrix numRowsB numColsB _)
    | numRowsA /= 4 || numColsA /= 1 = error "matrix is not a tuple"
    | numRowsB /= 4 || numColsB /= 1 = error "matrix is not a tuple"
    | otherwise = vector (y m * z n - z m * y n)
                         (z m * x n - x m * z n)
                         (x m * y n - y m * x n)

elemAt :: Int -> Int -> Matrix -> Double
elemAt row col (Matrix numRows numCols matrixData)
    | row < 0        = error msg
    | col < 0        = error msg
    | row >= numRows = error msg
    | col >= numCols = error msg
    | otherwise      = matrixData V.! ix
    where msg = "index out of bounds: (" ++ show row ++ ", " ++ show col ++ ")"
          ix = (row * numCols) + col

matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd (Matrix numRowsA numColsA dataA) (Matrix numRowsB numColsB dataB)
    | numRowsA /= numRowsB = error "number of rows not equal"
    | numColsA /= numColsB = error "number of columns not equal"
    | otherwise = Matrix numRowsA numColsA $ V.zipWith (+) dataA dataB

matrixSub :: Matrix -> Matrix -> Matrix
matrixSub (Matrix numRowsA numColsA dataA) (Matrix numRowsB numColsB dataB)
    | numRowsA /= numRowsB = error "number of rows not equal"
    | numColsA /= numColsB = error "number of columns not equal"
    | otherwise = Matrix numRowsA numColsA $ V.zipWith (-) dataA dataB

matrixMult :: Matrix -> Matrix -> Matrix
matrixMult a@(Matrix numRowsA numColsA dataA) b@(Matrix numRowsB numColsB dataB)
    | numColsA /= numRowsB = error "invalid matrix multiplication"
    | otherwise = Matrix numRowsA numColsB (runST compute)
    where
        compute = do
            result <- M.replicate (numRowsA * numColsB) 0.0
            -- for each row in A
            forM_ [0..numRowsA-1] $ \row -> do
                -- for each column in B
                forM_ [0..numColsB-1] $ \col -> do
                    -- sum the pairwise products of the vectors
                    sum <- newSTRef 0.0
                    forM_ [0..numColsA-1] $ \ix -> do
                        -- get value from row in A
                        let aTerm = elemAt row ix a
                        -- get value from col in B
                        let bTerm = elemAt ix col b
                        modifySTRef sum (+ (aTerm * bTerm))
                    total <- readSTRef sum
                    M.write result (row * numColsB + col) total
            V.unsafeFreeze result

identityMatrix4 = Matrix 4 4 $ V.fromList
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, 1
    ]

transpose :: Matrix -> Matrix
transpose m@(Matrix numRows numCols matrixData) =
    Matrix numCols numRows (runST compute)
    where compute :: ST s (V.Vector Double)
          compute = do
              result <- M.replicate (numRows * numCols) 0.0
              -- for each row
              forM_ [0..numRows-1] $ \row -> do
                  -- for each column
                  forM_ [0..numCols-1] $ \col -> do
                      -- copy the value to the new matrix
                      let val = elemAt row col m
                      M.write result (col * numRows + row) val
              V.unsafeFreeze result

determinant :: Matrix -> Double
determinant m@(Matrix numRows numCols matrixData)
    | numRows /= numCols = error "determinant only defined for square matrices"
    | numRows == 2       = (e 0 0) * (e 1 1) - (e 0 1) * (e 1 0)
    | otherwise =
        sum $ flip map [0..numCols-1] $ \col ->
            (e 0 col) * cofactor m 0 col
    where e r c = elemAt r c m

submatrix :: Matrix -> Int -> Int -> Matrix
submatrix (Matrix numRows numCols matrixData) row col =
        Matrix (numRows - 1) (numCols - 1) subData
    where copyList = [ r * numCols + c
                     | r <- [0..numRows-1], c <- [0..numCols-1]
                     , r /= row, c /= col
                     ]
          subData = V.backpermute matrixData (V.fromList copyList)

minor :: Matrix -> Int -> Int -> Double
minor matrix row col = determinant $ submatrix matrix row col

cofactor :: Matrix -> Int -> Int -> Double
cofactor matrix row col
    | odd (row + col) = negate (minor matrix row col)
    | otherwise       = minor matrix row col

invertable :: Matrix -> Bool
invertable = (not . floatsEq 0) . determinant

inverse :: Matrix -> Matrix
inverse m@(Matrix numRows numCols matrixData)
    | invertable m = result
    | otherwise    = error "matrix is not invertable"
    where ixs = V.fromList [(r,c) | r <- [0..numRows-1], c <- [0..numCols-1]]
          vals = V.map (\(r,c) -> cofactor m r c / determinant m) ixs
          result = transpose $ Matrix numCols numRows vals
