module Intcode (readProgramFromStr, handleProgram, handleProgramStreams, readProgram, writeProgram) where

import Data.List
import Data.Ord
import Text.Regex

type Mode = Int
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
    | inst == 1 = handleProgramStreams (opcodeAdd program o1 o2 t) is os (t+4)
    | inst == 2 = handleProgramStreams (opcodeMult program o1 o2 t) is os (t+4)
    | inst == 3 = let res = opcodeInput program is t in handleProgramStreams (fst res) (snd res) os (t+2)
    | inst == 4 = handleProgramStreams program is (opcodeOutput program os o1 t) (t+2)
    | otherwise = error ("Invalid opcode at position " ++ (show t) ++ ": " ++ (show inst))
    where op = readProgram program t 1
          inst = mod op 100
          o1 = mod (div op 100) 10
          o2 = mod (div op 1000) 10
          o3 = div op 10000

opcodeAdd :: Program -> Mode -> Mode -> Int -> Program
opcodeAdd program o1 o2 t = writeProgram program (readProgram program (t+3) 1) ((readProgram program (t+1) o1) + (readProgram program (t+2) o2))

opcodeMult :: Program -> Mode -> Mode -> Int -> Program
opcodeMult program o1 o2 t = writeProgram program (readProgram program (t+3) 1) ((readProgram program (t+1) o1) * (readProgram program (t+2) o2))

opcodeInput :: Program -> IStream -> Int -> (Program,IStream)
opcodeInput program is t = let str = readStream is in (writeProgram program (readProgram program (t+1) 1) (fst str),snd str)
                               
opcodeOutput :: Program -> OStream -> Mode -> Int -> OStream
opcodeOutput program os o1 t = writeStream os (readProgram program(t+1) o1)
    
readStream :: IStream -> (Data,IStream)
readStream [] = error "attempted to read from empty input stream"
readStream (i:is) = (i,is)

writeStream :: OStream -> Data -> OStream
writeStream os d = d:os

readProgram :: Program -> Position -> Mode -> Opcode
readProgram program pos mode
    | null matches = -9999999
    | mode == 1 = fst $ head matches                            -- immediate mode
    | mode == 0 = readProgram program (fst $ head matches) 1    -- position mode
    where matches = filter (\(_,p) -> (pos == p)) program

writeProgram :: Program -> Position -> Opcode -> Program
writeProgram program pos val = (val,pos) : (remFromList ((readProgram program pos 1,pos)) program)
    
remFromList :: (Eq a) => a -> [a] -> [a]
remFromList _ [] = []
remFromList x (y:ys)
    | x == y = remFromList x ys
    | otherwise = y : remFromList x ys
    
sortProgram :: Program -> Program
sortProgram program = sortBy (comparing $ snd) program

splitAtCommas :: String -> [String]
splitAtCommas a = splitRegex (mkRegex "\\,") a
