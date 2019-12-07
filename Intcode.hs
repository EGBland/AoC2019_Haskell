module Intcode (readProgramFromStr, handleProgram, readProgram, writeProgram) where

import Data.List
import Data.Ord
import Text.Regex

type Opcode = Int
type Position = Int
type Instruction = (Opcode,Position)
type Program = [Instruction]

readProgramFromStr :: String -> Program
readProgramFromStr s = zip [read a :: Opcode | a <- splitAtCommas s] [0..]

handleProgram :: Program -> Int -> Program
handleProgram program t
    | inst == 99 = sortProgram program
    | inst == 1 = handleProgram (opcodeAdd program t) (t+4)
    | inst == 2 = handleProgram (opcodeMult program t) (t+4)
    | otherwise = error ("Invalid opcode at position " ++ (show t) ++ ": " ++ (show inst))
    where inst = readProgram program t

opcodeAdd :: Program -> Int -> Program
opcodeAdd program t = writeProgram program (readProgram program (t+3)) ((readProgram program (readProgram program (t+1))) + (readProgram program (readProgram program (t+2))))

opcodeMult :: Program -> Int -> Program
opcodeMult program t = writeProgram program (readProgram program (t+3)) ((readProgram program (readProgram program (t+1))) * (readProgram program (readProgram program (t+2))))
    
readProgram :: Program -> Position -> Opcode
readProgram program pos
    | null matches = -9999999
    | otherwise = fst $ head matches
    where matches = filter (\(_,p) -> (pos == p)) program

writeProgram :: Program -> Position -> Opcode -> Program
writeProgram program pos val = (val,pos) : (remFromList ((readProgram program pos,pos)) program)
    
remFromList :: (Eq a) => a -> [a] -> [a]
remFromList _ [] = []
remFromList x (y:ys)
    | x == y = remFromList x ys
    | otherwise = y : remFromList x ys
    
sortProgram :: Program -> Program
sortProgram program = sortBy (comparing $ snd) program

splitAtCommas :: String -> [String]
splitAtCommas a = splitRegex (mkRegex "\\,") a
