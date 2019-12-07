import Data.List
import Data.Ord
import Text.Regex

type Opcode = Int
type Position = Int
type Instruction = (Opcode,Position)
type Program = [Instruction]

main = do
    day2_1
    day2_2

day2_1 = do
    input <- readFile "input/day2.txt"
    let ins = [read a :: Opcode | a <- splitAtCommas input]
    let program = zip (take 1 ins ++ [12,2] ++ drop 3 ins) [0..length ins-1]
    putStrLn ("Day 02, Problem 1: " ++ (show $ fst $ head $ sortProgram $ handleProgram program 0))

day2_2 = do
    input <- readFile "input/day2.txt"
    let ins = [read a :: Opcode | a <- splitAtCommas input]
    let programs = [(zip (take 1 ins ++ [noun,verb] ++ drop 3 ins) [0..length ins-1],(noun,verb)) | noun<-[0..99], verb<-[0..99]]
    let resolvedPrograms = [(handleProgram program 0,nv) | (program,nv) <- programs]
    putStrLn ("Day 02, Problem 2: " ++ (show $ let (_,nv) = head $ filter (\(prog,_) -> (readProgram prog 0 == 19690720)) resolvedPrograms in (100*fst nv) + snd nv))

splitAtCommas :: String -> [String]
splitAtCommas a = splitRegex (mkRegex "\\,") a

handleProgram :: Program -> Int -> Program
handleProgram program t
    | inst == 99 = program
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
