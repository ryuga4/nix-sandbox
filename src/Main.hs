module Main where

import Data.List


import Data.Ratio
import Debug.Trace

main :: IO ()
main = do
  putStrLn "Wprowadź macierz"
  matrix <- getMatrix []
  printMatrix $ rowReduce 0 0 matrix

getMatrix :: Matrix -> IO Matrix
getMatrix m = do
  line <- getLine
  if line == ""
    then return m
    else getMatrix (m ++ [map (fromIntegral . read) $ words line])



type Matrix = [[Rational]]


example :: Matrix
example = [ [1,2,3,4]
          , [2,1,5,2]
          , [1,2,1,5]
          ]

example2 :: Matrix
example2 = [ [0,0,3,4]
           , [0,1,5,2]
           , [0,2,1,5]
           ]

matrixToString :: Matrix -> String
matrixToString [] = "\n"
matrixToString (m:ms) =
  let row = foldl (\a b -> a ++ " | " ++ show (fromRational b)) "" m
  in row ++ "\n" ++ matrixToString ms

printMatrix :: Matrix -> IO ()
printMatrix [] = putStrLn "\n"
printMatrix (m:ms) = do
  mapM_ (\i -> putStr $ show (fromRational i :: Double) ++ " | ") m
  putStr "\n"
  printMatrix ms


------------------------------- Reducer


swapRows :: Int -> Int -> Matrix -> Matrix
swapRows a b m | a > b = swapRows b a m
               | a == b = m
               | otherwise = take a m ++ [m !! b] ++ (take (b - a - 1) (drop (a+1) m)) ++ [m !! a] ++ drop (b+1) m

-- >>> swapRows 0 2 example
-- [[1 % 1,2 % 1,1 % 1,5 % 1],[2 % 1,1 % 1,5 % 1,2 % 1],[1 % 1,2 % 1,3 % 1,4 % 1]]


-- >>> swapRows 1 2 example
-- [[1 % 1,2 % 1,3 % 1,4 % 1],[1 % 1,2 % 1,1 % 1,5 % 1],[2 % 1,1 % 1,5 % 1,2 % 1]]


multRow :: Int -> Rational -> Matrix -> Matrix
multRow _ 0 _ = error "multRow error"
multRow n x m = take n m ++ [map (*x) (m !! n)] ++ drop (n+1) m

-- >>> multRow 1 100 example
-- [[1 % 1,2 % 1,3 % 1,4 % 1],[200 % 1,100 % 1,500 % 1,200 % 1],[1 % 1,2 % 1,1 % 1,5 % 1]]


addMultRow :: Int -> Int -> Rational -> Matrix -> Matrix
addMultRow a b x m | a > b = swapRows a b $ addMultRow b a x $ swapRows a b m
                   | a == b = multRow a x m
                   | otherwise = take b m ++ [zipWith (+) (map (*x) $ m !! a) (m !! b)] ++ drop (b+1) m

-- >>> addMultRow 0 2 20 example
-- [[1 % 1,2 % 1,3 % 1,4 % 1],[2 % 1,1 % 1,5 % 1,2 % 1],[21 % 1,42 % 1,61 % 1,85 % 1]]




-- >>> addMultRow 1 2 100 example
-- [[1 % 1,2 % 1,3 % 1,4 % 1],[2 % 1,1 % 1,5 % 1,2 % 1],[201 % 1,102 % 1,501 % 1,205 % 1]]


firstNotZeroColl :: Int -> Matrix -> Maybe Int
firstNotZeroColl _ [] = Nothing
firstNotZeroColl n (r:rs) | n > length r = error "firstNotZeroColl error"
                         | r !! n == 0 = (+1) <$> firstNotZeroColl n rs
                         | otherwise   = Just 0

-- >>> firstNotZeroColl 0 example
-- Just 0

-- >>> firstNotZeroColl 1 example2
-- Just 1

-- >>> firstNotZeroColl 0 example2
-- Nothing

numberOfZeroRows :: Matrix -> Int
numberOfZeroRows m = length $ takeWhile (all (==0)) $ reverse m




rowReduce :: Int -> Int -> Matrix -> Matrix
rowReduce n l m | l + numberOfZeroRows m == length m = m
rowReduce n l m = case firstNotZeroColl n m of
                  Nothing -> rowReduce (n+1) l m
                  Just x  ->
                    let m1 = swapRows 0 l m
                        bottomRows = take (length m - l - 1) $ reverse m
                        m2 = foldl (\acc (row, rowN) -> addMultRow l rowN (negate (row !! l) / (acc !! l !! n)) acc) m (zip bottomRows [length m- 1, length m - 2..])
                        m3 = multRow l (1 / (m !! l !! n)) m2
                        m4 = foldl (\acc (rowN, row) -> addMultRow l rowN (negate (row !! l) / (acc !! l !! n)) acc) m3 (zip [0..l-1] m3)
                    in rowReduce (n+1) (l+1) m4

