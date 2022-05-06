type Matrix = [[Int]]
type Column = [Int]
type Row = [Int]

-- Helpers------------------------------------------------------------------
extractColumn :: Matrix -> Int -> Column
extractColumn [] col = []
extractColumn (x:xs) col = (x !! (col-1)):(extractColumn xs col)

removeColumn :: Matrix -> Int -> Matrix
removeColumn xs rem = [((take (fromIntegral (rem-1)) y) ++ (drop rem y)) |y <- xs]

scalarProduct :: Row -> Row -> Int
scalarProduct x y = foldr (+) 0 (zipWith (*) x y)   

rowMul :: Row -> Matrix -> Row
rowMul x xs = [scalarProduct x y | y <- xs]

mmul :: Matrix -> Matrix -> Matrix
mmul x y = [(rowMul z y)| z <- x]

matlen :: Matrix -> Int
matlen x = foldr (+) 0 [ 1 | _ <- (x !! 0)]

-- Operations------------------------------------------------------------------

transpose :: Matrix -> Matrix
transpose [] = []
transpose x = [(extractColumn x y)| y<- [1..(matlen x)]]

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply x y = mmul x (transpose y)

-- Cofactor and Minor Method 
-- det[det[...[x0]]] - det[det[...[x1]]] + det[det[...[x2]]] - ...
determinant :: Matrix -> Int
determinant [] = 0
determinant [[x]] = x
determinant m = sum [(-1)^cof*x*(determinant (removeColumn (drop 1 m) (cof+1)))| (cof, x) <- zip[0..] (m !! 0)]

-- Test values------------------------------------------------------------------
-- m1 :: [[Int]]
-- m1 = [[1,2,3],[4,5,6]]
-- m2 :: [[Int]]
-- m2 = [[1,2],[3,4]]
-- m3 :: [[Int]]
-- m3 = [[5,6],[7,8]]
-- m4 :: [[Int]]
-- m4 = [[2,3,4],[5,6,7]]
-- m5 :: [[Int]]
-- m5 = [[1,2,3,4],[2,3,5,6],[7,1,2,4]]
-- m6 :: [[Int]]
-- m6 = [[4,6],[3,8]]
-- m7 :: [[Int]]
-- m7 = [[99]]
-- m8 :: [[Int]]
-- m8 = []
-- m9 :: [[Int]]
-- m9 = [[6,1,1],[4,-2,5],[2,8,7]]