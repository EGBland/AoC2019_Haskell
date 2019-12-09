import Intcode

main = do
    day9_1
    day9_2

day9_1 = do
    input <- readFile "input/day9.txt"
    putStrLn ("Day 09, Problem 1: " ++ (show $ snd $ runProgram (readProgramFromStr input) [1] [] 0 0))

day9_2 = do
    input <- readFile "input/day9.txt"
    putStrLn ("Day 09, Problem 2: " ++ (show $ snd $ runProgram (readProgramFromStr input) [2] [] 0 0))
