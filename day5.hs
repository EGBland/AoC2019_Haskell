import Intcode

main = do
    day5_1
    day5_2
    
day5_1 = do
    input <- readFile "day5.txt"
    let program = readProgramFromStr input
    putStrLn ("Day 05, Problem 1: " ++ (show $ head $ snd $ handleProgramStreams program [1] [] 0))
    
day5_2 = do
    input <- readFile "day5.txt"
    let program = readProgramFromStr input
    putStrLn ("Day 05, Problem 2: " ++ (show $ head $ snd $ handleProgramStreams program [5] [] 0))
