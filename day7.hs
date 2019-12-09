import Data.List

import Intcode

type ProgramFunction = IStream -> OStream -> Caret -> RelBase -> (Program,OStream)
type Phase = Integer
type Input = Integer
type Output = Integer

main = do
    day7_1
    --day7_2

day7_1 = do
    input <- readFile "input/day7.txt"
    let program = readProgramFromStr input
    
    let pset = permutations [0..4]
    let amps = take 5 (repeat $ runProgram program)
    putStrLn ("Day 07, Problem 1: " ++ (show $ maximum [runChain amps p 0 | p <- pset]))

day7_2 = putStrLn "Hello world!"
    
runChain :: [ProgramFunction] -> [Phase] -> Input -> Output
runChain [] _ input = input
runChain _ [] input = input
runChain (amp:amps) (phase:phases) input = runChain amps phases (head $ snd $ amp [phase,input] [] 0 0)
