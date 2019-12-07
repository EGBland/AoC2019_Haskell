import Data.List
import Text.Regex

type Orbit = (String,String)
type Object = (String,[String])

main = do
    day6_1
    day6_2

day6_1 = do
    input <- readFile "day6.txt"
    let orbits = [(head $ splitAtOp a ")", last $ splitAtOp a ")") | a <- lines input]
    let universe = getUniverse orbits
    let dirOrbits = [(o, getDirOrbs o orbits) | o <- universe]
    putStrLn ("Day 06, Problem 1: " ++ (show $ sum [countIndirOrbs a dirOrbits | a <- dirOrbits]))

day6_2 = do
    input <- readFile "day6.txt"
    let orbits = [(head $ splitAtOp a ")", last $ splitAtOp a ")") | a <- lines input]
    let universe = getUniverse orbits
    let dirOrbits = [(o, getDirOrbs o orbits) | o <- universe]
    let you = reverse $ pathToCom "YOU" dirOrbits
    let san = reverse $ pathToCom "SAN" dirOrbits
    let cmn = commonPath you san
    putStrLn ("Day 06, Problem 2: " ++ (show $ ((length you) + (length san) - 2*(length cmn))))

splitAtOp :: String -> String -> [String]
splitAtOp a op = splitRegex (mkRegex ("\\"++op)) a

getUniverse :: [Orbit] -> [String]
getUniverse orbits = nub $ concat [map (\(a,_) -> a) orbits, map (\(_,b) -> b) orbits]

getDirOrbs :: String -> [Orbit] -> [String]
getDirOrbs a orbits = map (\(_,y) -> y) $ filter (\(x,_) -> x == a) orbits

dirOrbBy :: Object -> String -> Bool
dirOrbBy (_,orbs) b = elem b orbs

indirOrbBy :: Object -> String -> [Object] -> Bool
indirOrbBy _ _ [] = False
indirOrbBy (a,orbs) b orbits = dirOrbBy (a,orbs) b || indirOrbBy' orbs b orbits

indirOrbBy' :: [String] -> String -> [Object] -> Bool
indirOrbBy' [] _ _ = False
indirOrbBy' _ _ [] = False
indirOrbBy' (a:as) b orbits = let a' = head $ filter (\(x,_) -> x == a) orbits in indirOrbBy a' b orbits || indirOrbBy' as b orbits

countDirOrbs :: Object -> Int
countDirOrbs (_,orbs) = length orbs

countIndirOrbs :: Object -> [Object] -> Int
countIndirOrbs _ [] = 0
countIndirOrbs (a,orbs) orbits = countDirOrbs (a,orbs) + sum [countIndirOrbs b orbits | b <- map (\x -> (head $ filter (\(y,_) -> y == x) orbits)) orbs]

pathToCom :: String -> [Object] -> [String]
pathToCom a orbits
    | a == "COM" = []
    | otherwise = b : pathToCom b orbits
    where b = fst $ head $ filter (\(_,orbs2) -> elem a orbs2) orbits

commonPath :: [String] -> [String] -> [String]
commonPath a b = [c | c <- a, elem c b]
