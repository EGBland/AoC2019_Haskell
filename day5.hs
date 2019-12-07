import Intcode

main = do
    day5_1
    
day5_1 = do
    input <- readFile "day5.txt"
    let program = readProgramFromStr "3,0,4,0,99"
    putStrLn $ show $ handleProgramStreams program [1,2] [] 0
    
