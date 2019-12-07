import Data.List
import Text.Regex

main = do
    day4_1
    day4_2

day4_1 = do
    input <- readFile "day4.txt"
    let dmin = read (head $ splitAtHyphen input) :: Int
    let dmax = read (last $ splitAtHyphen input) :: Int
    putStrLn ("Day 04, Problem 1: " ++ (show $ length $ filter (\x -> dmin <= x && x <= dmax) [a*10^5 + b*10^4 + c*10^3 + d*10^2 + e*10 + f | a <- [0..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9], f <- [e..9], (length $ nub [a,b,c,d,e,f]) < (length [a,b,c,d,e,f])]))

day4_2 = do
    input <- readFile "day4.txt"
    let dmin = read (head $ splitAtHyphen input) :: Int
    let dmax = read (last $ splitAtHyphen input) :: Int
    putStrLn ("Day 04, Problem 2: " ++ (show $ length $ filter (\(x,a) -> dmin <= x && x <= dmax && validInstances a) [(a*10^5 + b*10^4 + c*10^3 + d*10^2 + e*10 + f,[a,b,c,d,e,f]) | a <- [0..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9], f <- [e..9]]))

validInstances :: [Int] -> Bool
validInstances a = any (\x -> instCt x a == 2) a

-- copied from some of my other code
instCt :: Eq a => a -> [a] -> Int
instCt x = length.filter (x==)

splitAtHyphen :: String -> [String]
splitAtHyphen a = splitRegex (mkRegex "\\-") a
