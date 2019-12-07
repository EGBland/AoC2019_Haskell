import Text.Regex

import Intcode

main = do
    day2_1
    day2_2

day2_1 = do
    input <- readFile "input/day2.txt"
    let program = writeProgram (writeProgram (readProgramFromStr input) 1 12) 2 2
    putStrLn ("Day 02, Problem 1: " ++ (show $ fst $ head $ handleProgram program 0))

day2_2 = do
    input <- readFile "input/day2.txt"
    let program = readProgramFromStr input
    let programs = [(writeProgram (writeProgram program 1 n) 2 v,100*n+v) | n <- [0..99], v <- [0..99]]
    let resolvedPrograms = [(handleProgram prog 0,nv) | (prog,nv) <- programs]
    putStrLn ("Day 02, Problem 2: " ++ (show $ snd $ head $ filter (\(prog,_) -> (readProgram prog 0 == 19690720)) resolvedPrograms))
