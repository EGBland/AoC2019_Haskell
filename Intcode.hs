module Intcode (readProgramFromStr, handleProgram, handleProgramStreams, readProgram, writeProgram) where

import Data.List
import Data.Ord
import Text.Regex

type Mode = Integer
type Data = Integer
type Opcode = Integer
type Position = Integer
type Instruction = (Opcode,Position)
type Program = [Instruction]
type RelBase = Integer
type Caret = Integer

type IStream = [Data]
type OStream = [Data]

readProgramFromStr :: String -> Program
readProgramFromStr s = zip [read a :: Opcode | a <- splitAtCommas s] [0..]

handleProgram :: Program -> Caret -> Program
handleProgram program t = fst $ handleProgramStreams program [] [] t 0

handleProgramStreams :: Program -> IStream -> OStream -> Caret -> RelBase -> (Program,OStream)
handleProgramStreams program is os t base
    | inst == 99 = (sortProgram program,os)
    | inst == 1 = handleProgramStreams (opcodeAdd program o1 o2 o3 t base) is os (t+4) base
    | inst == 2 = handleProgramStreams (opcodeMult program o1 o2 o3 t base) is os (t+4) base
    | inst == 3 = let res = opcodeInput program is o1 t base in handleProgramStreams (fst res) (snd res) os (t+2) base 
    | inst == 4 = handleProgramStreams program is (opcodeOutput program os o1 t base) (t+2) base
    | inst == 5 = handleProgramStreams program is os (opcodeJumpIfTrue program o1 o2 t base) base
    | inst == 6 = handleProgramStreams program is os (opcodeJumpIfFalse program o1 o2 t base) base
    | inst == 7 = handleProgramStreams (opcodeLessThan program o1 o2 o3 t base) is os (t+4) base
    | inst == 8 = handleProgramStreams (opcodeEquals program o1 o2 o3 t base) is os (t+4) base
    | inst == 9 = handleProgramStreams program is os (t+2) (opcodeIncRelBase program o1 t base)
    | otherwise = error ("Invalid opcode at position " ++ (show t) ++ ": " ++ (show inst))
    where op = readProgram program t 1 base
          inst = mod op 100
          o1 = mod (div op 100) 10
          o2 = mod (div op 1000) 10
          o3 = div op 10000

opcodeAdd :: Program -> Mode -> Mode -> Mode -> Caret -> RelBase -> Program
opcodeAdd program o1 o2 o3 t base
    | o3 == 0 = writeProgram program (readProgram program (t+3) 1 base) ((readProgram program (t+1) o1 base) + (readProgram program (t+2) o2 base))
    |o3 == 2 = writeProgram program (base + (readProgram program (t+3) 1 base)) ((readProgram program (t+1) o1 base) + (readProgram program (t+2) o2 base))

opcodeMult :: Program -> Mode -> Mode -> Mode -> Caret -> RelBase -> Program
opcodeMult program o1 o2 o3 t base
    | o3 == 0 = writeProgram program (readProgram program (t+3) 1 base) ((readProgram program (t+1) o1 base) * (readProgram program (t+2) o2 base))
    | o3 == 2 = writeProgram program (base + (readProgram program (t+3) 1 base)) ((readProgram program (t+1) o1 base) * (readProgram program (t+2) o2 base))

opcodeInput :: Program -> IStream -> Mode -> Caret -> RelBase -> (Program,IStream)
opcodeInput program is o1 t base
    | o1 == 0 = (writeProgram program (readProgram program (t+1) 1 base) (fst str),snd str)
    | o1 == 2 = (writeProgram program (base + (readProgram program (t+1) 1 base)) (fst str),snd str)
    where str = readStream is
                               
opcodeOutput :: Program -> OStream -> Mode -> Caret -> RelBase -> OStream
opcodeOutput program os o1 t base = writeStream os (readProgram program (t+1) o1 base)

opcodeJumpIfTrue :: Program -> Mode -> Mode -> Caret -> RelBase -> Caret
opcodeJumpIfTrue program o1 o2 t base = if readProgram program (t+1) o1 base == 0 then (t+3) else readProgram program (t+2) o2 base

opcodeJumpIfFalse :: Program -> Mode -> Mode -> Caret -> RelBase -> Caret
opcodeJumpIfFalse program o1 o2 t base = if readProgram program (t+1) o1 base /= 0 then (t+3) else readProgram program (t+2) o2 base

opcodeLessThan :: Program -> Mode -> Mode -> Mode -> Caret -> RelBase -> Program
opcodeLessThan program o1 o2 o3 t base
    | o3 == 0 = let f = writeProgram program (readProgram program (t+3) 1 base) in if (readProgram program (t+1) o1 base) < (readProgram program (t+2) o2 base) then f 1 else f 0
    | o3 == 2 = let f = writeProgram program (base + (readProgram program (t+3) 1 base)) in if (readProgram program (t+1) o1 base) < (readProgram program (t+2) o2 base) then f 1 else f 0

opcodeEquals :: Program -> Mode -> Mode -> Mode -> Caret -> RelBase -> Program
opcodeEquals program o1 o2 o3 t base
    | o3 == 0 = let f = writeProgram program (readProgram program (t+3) 1 base) in if (readProgram program (t+1) o1 base) == (readProgram program (t+2) o2 base) then f 1 else f 0
    | o3 == 2 = let f = writeProgram program (base + (readProgram program (t+3) 1 base)) in if (readProgram program (t+1) o1 base) == (readProgram program (t+2) o2 base) then f 1 else f 0

opcodeIncRelBase :: Program -> Mode -> Caret -> RelBase -> RelBase
opcodeIncRelBase program o1 t base = base + (readProgram program (t+1) o1 base)

readStream :: IStream -> (Data,IStream)
readStream [] = error "attempted to read from empty input stream"
readStream (i:is) = (i,is)

writeStream :: OStream -> Data -> OStream
writeStream os d = d:os

readProgram :: Program -> Position -> Mode -> RelBase -> Opcode
readProgram program pos mode base
    | pos < 0 = error ("attempted to access negative memory. (" ++ (show pos) ++ "; " ++ (show base) ++ ")")
    | null matches = 0
    | mode == 2 = readProgram program ((fst $ head matches) + base) 1 base -- relative mode
    | mode == 1 = fst $ head matches                                 -- immediate mode
    | mode == 0 = readProgram program (fst $ head matches) 1 base    -- position mode
    where matches = filter (\(_,p) -> (pos == p)) program

writeProgram :: Program -> Position -> Opcode -> Program
writeProgram program pos val = (val,pos) : (remFromList ((readProgram program pos 1 0,pos)) program)
    
remFromList :: (Eq a) => a -> [a] -> [a]
remFromList _ [] = []
remFromList x (y:ys)
    | x == y = remFromList x ys
    | otherwise = y : remFromList x ys
    
sortProgram :: Program -> Program
sortProgram program = sortBy (comparing $ snd) program

splitAtCommas :: String -> [String]
splitAtCommas a = splitRegex (mkRegex "\\,") a
