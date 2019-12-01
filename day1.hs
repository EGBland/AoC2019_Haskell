import System.IO
import Control.Monad

main = do 
    day1_1
    day1_2

day1_1 = do
    contents <- readFile "day1.txt"
    let fuel = sum [getFuel (read a :: Int) | a <- words contents]
    putStrLn $ concat ["Problem 1: ", show fuel]

day1_2 = do
    contents <- readFile "day1.txt"
    let fuel = sum [getFuelRecursive (read a :: Int) | a <- words contents]
    putStrLn $ concat ["Problem 2: ", show fuel]

getFuel :: Int -> Int
getFuel k = max 0 (div k 3 - 2)

getFuelRecursive :: Int -> Int
getFuelRecursive k
    | f == 0 = 0
    | otherwise = f + getFuelRecursive f
    where f = getFuel k
