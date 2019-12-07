import Data.List
import Text.Regex

type Orbit = (String,String)
type Object = (String,[String])

main = do
    input <- readFile "day6.txt"
    let orbits = [(head $ splitAtOp a ")", last $ splitAtOp a ")") | a <- lines input]
    let universe = getUniverse orbits
    let dirOrbits = [(o, getDirOrbs o orbits) | o <- universe]
    putStrLn $ show $ sum [countIndirOrbs a dirOrbits | a <- dirOrbits]

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