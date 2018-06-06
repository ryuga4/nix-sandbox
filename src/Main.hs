module Main where

import Data.List


import Data.Ratio
import Debug.Trace

main :: IO ()
main = do
  putStrLn "Wprowadź macierz"
  matrix <- getMatrix []
  putStrLn $ matrixToString $ rowReduce 0 0 matrix

type Matrix = [[Rational]]

getMatrix :: Matrix -> IO Matrix
getMatrix m = do
  line <- getLine
  if line == ""
    then return m
    else getMatrix (m ++ [map (fromIntegral . read) $ words line])

matrixToString :: Matrix -> String
matrixToString [] = "\n"
matrixToString (m:ms) =
  let row = foldl (\a b -> a ++ " | " ++ show (fromRational b)) "" m
  in row ++ "\n" ++ matrixToString ms

remove :: Int -> [a] -> [a]
remove _ []     = []
remove 0 (_:as) = as
remove n (a:as) = a : remove (n-1) as

swapRows :: Int -> Int -> Matrix -> Matrix
swapRows a b m | a > b = swapRows b a m
               | a == b = m
               | otherwise = take a m ++ [m !! b] ++ take (b - a - 1) (drop (a+1) m) ++ [m !! a] ++ drop (b+1) m

multRow :: Int -> Rational -> Matrix -> Matrix
multRow _ 0 _ = error "multRow error"
multRow n x m = take n m ++ [map (*x) (m !! n)] ++ drop (n+1) m

addMultRow :: Int -> Int -> Rational -> Matrix -> Matrix
addMultRow a b x m | a > b = swapRows a b $ addMultRow b a x $ swapRows a b m
                   | a == b = multRow a x m
                   | otherwise = take b m ++ [zipWith (+) (map (*x) $ m !! a) (m !! b)] ++ drop (b+1) m

firstNotZeroColl :: Int -> Matrix -> Maybe Int
firstNotZeroColl _ [] = Nothing
firstNotZeroColl n (r:rs) | n > length r = error "firstNotZeroColl error"
                         | r !! n == 0 = (+1) <$> firstNotZeroColl n rs
                         | otherwise   = Just 0

numberOfZeroRows :: Matrix -> Int
numberOfZeroRows m = length $ takeWhile (all (==0)) $ reverse m

rowReduce :: Int -> Int -> Matrix -> Matrix
rowReduce n l m | l + numberOfZeroRows m == length m = m
rowReduce n l m = case firstNotZeroColl n (drop l m) of
                  Nothing -> rowReduce (n+1) l m
                  Just x  ->
                    let x' = x + l
                        m2 = swapRows l x' m
                        zippedRows = remove l $ zip [0..] m
                        m3 = foldl (\acc (rowN, row) -> addMultRow l rowN (negate (row !! n) / (acc !! l !! n)) acc) m2 zippedRows
                        m4 = multRow l (1 / m3 !! l !! n) m3
                     in rowReduce (n+1) (l+1) m4
