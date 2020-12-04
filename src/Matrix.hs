
module Matrix where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Control.Monad
import Data.STRef

import Tuple

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

elemAt :: Int -> Int -> Matrix -> Double
elemAt row col (Matrix numRows numCols matrixData)
    | row < 0        = error msg
    | col < 0        = error msg
    | row >= numRows = error msg
    | col >= numCols = error msg
    | otherwise      = matrixData V.! ix
    where msg = "index out of bounds: (" ++ show row ++ ", " ++ show col ++ ")"
          ix = (row * numCols) + col


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

tupleToMatrix :: Tuple -> Matrix
tupleToMatrix (x, y, z, w) = Matrix 4 1 $ V.fromList [x, y, z, w]

matrixToTuple :: Matrix -> Tuple
matrixToTuple (Matrix numRows numCols matrixData)
    | numRows /= 4 || numCols /= 1
        = error "only matrices of size 4x1 may be converted"
    | otherwise = let (x:y:z:w:[]) = V.toList matrixData in (x, y, z, w)

matrixTupleMult :: Matrix -> Tuple -> Tuple
matrixTupleMult m t = matrixToTuple $ matrixMult m (tupleToMatrix t)

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
