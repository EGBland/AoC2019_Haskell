module Intcode (readProgramFromStr, handleProgram, handleProgramStreams, readProgram, writeProgram) where

import Data.List
import Data.Ord
import Text.Regex

type Data = Int
type Opcode = Int
type Position = Int
type Instruction = (Opcode,Position)
type Program = [Instruction]

type IStream = [Data]
type OStream = [Data]

readProgramFromStr :: String -> Program
readProgramFromStr s = zip [read a :: Opcode | a <- splitAtCommas s] [0..]

handleProgram :: Program -> Int -> Program
handleProgram program t = fst $ handleProgramStreams program [] [] t

handleProgramStreams :: Program -> IStream -> OStream -> Int -> (Program,OStream)
handleProgramStreams program is os t
    | inst == 99 = (sortProgram program,os)
    | inst == 1 = handleProgramStreams (opcodeAdd program t) is os (t+4)
    | inst == 2 = handleProgramStreams (opcodeMult program t) is os (t+4)
    | inst == 3 = let res = opcodeInput program is t in handleProgramStreams (fst res) (snd res) os (t+2)
    | inst == 4 = handleProgramStreams program is (opcodeOutput program os t) (t+2)
    | otherwise = error ("Invalid opcode at position " ++ (show t) ++ ": " ++ (show inst))
    where inst = readProgram program t

opcodeAdd :: Program -> Int -> Program
opcodeAdd program t = writeProgram program (readProgram program (t+3)) ((readProgram program (readProgram program (t+1))) + (readProgram program (readProgram program (t+2))))

opcodeMult :: Program -> Int -> Program
opcodeMult program t = writeProgram program (readProgram program (t+3)) ((readProgram program (readProgram program (t+1))) * (readProgram program (readProgram program (t+2))))

opcodeInput :: Program -> IStream -> Int -> (Program,IStream)
opcodeInput program is t = let str = readStream is in (writeProgram program (readProgram program (t+1)) (fst str),snd str)
                               
opcodeOutput :: Program -> OStream -> Int -> OStream
opcodeOutput program os t = writeStream os (readProgram program (readProgram program (t+1)))
    
readStream :: IStream -> (Data,IStream)
readStream [] = error "attempted to read from empty input stream"
readStream (i:is) = (i,is)

writeStream :: OStream -> Data -> OStream
writeStream os d = d:os

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
