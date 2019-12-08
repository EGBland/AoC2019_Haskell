import Text.Regex
import Data.List

type Point = (Int,Int)
type Line = (Point,Point)
data Orientation = Horizontal | Vertical deriving (Eq)

main = do
    day3_1
    day3_2

day3_1 = do
    input <- readFile "input/day3.txt"
    putStrLn ("Day 03, Problem 1: " ++ (show $ minimum $ map manhattan (getIntersects (processInstructions $ head $ lines input) (processInstructions $ last $ lines input))))

day3_2 = do
    input <- readFile "input/day3.txt"
    let w1 = processInstructions $ head $ lines input
    let w2 = processInstructions $ last $ lines input
    putStrLn ("Day 03, Problem 2: " ++ (show $ minimum $ map (\p -> ((stepsToPoint w1 p) + (stepsToPoint w2 p))) (getIntersects w1 w2)))
    
manhattan :: Point -> Int
manhattan p = (abs $ fst p) + (abs $ snd p)

stepsToPoint :: [Line] -> Point -> Int
stepsToPoint [] _ = error "point not on path"
stepsToPoint (l:ls) p = if pointOnLine l p then lineToPoint l p else (lineLength l) + (stepsToPoint ls p)

lineToPoint :: Line -> Point -> Int
lineToPoint l (x,y)
    | getOrient l == Horizontal = abs (x - x1)
    | getOrient l == Vertical = abs (y - y1)
    where x1 = fst $ fst l
          y1 = snd $ fst l

lineLength :: Line -> Int
lineLength ((xl,yb),(xr,yt)) = if getOrient ((xl,yb),(xr,yt)) == Vertical then abs (yt-yb) else abs (xr-xl)

pointOnLine :: Line -> Point -> Bool
pointOnLine l (x,y)
    | getOrient ((xl,yb),(xr,yt)) == Vertical = xl == x && yb <= y && y <= yt
    | otherwise = yb == y && xl <= x && x <= xr
    where xl = min (fst $ fst l) (fst $ snd l)
          xr = max (fst $ fst l) (fst $ snd l)
          yb = min (snd $ fst l) (snd $ snd l)
          yt = max (snd $ fst l) (snd $ snd l)

sortLines :: [Line] -> [Line]
sortLines [] = []
sortLines (((x1,y1),(x2,y2)):ls) = (if y1 > y2 || x1 > x2 then ((x2,y2),(x1,y1)) else ((x1,y1),(x2,y2))) : sortLines ls

getIntersects :: [Line] -> [Line] -> [Point]
getIntersects [] _ = []
getIntersects _ [] = []
getIntersects as (b:bs) = (getIntersects' as b) ++ (getIntersects as bs)

getIntersects' :: [Line] -> Line -> [Point]
getIntersects' [] _ = []
getIntersects' (a:as) b
    | getOrient a == getOrient b = getIntersects' as b
    | getOrient b == Vertical   = if axl <= bxl && bxl <= axr && byb <= ayb && ayb <= byt then (bxl,ayb) : getIntersects' as b else getIntersects' as b
    | getOrient b == Horizontal = if ayb <= byb && byb <= ayt && bxl <= axl && axl <= bxr then (axl,byb) : getIntersects' as b else getIntersects' as b
    where axl = min (fst $ fst $ a) (fst $ snd $ a)
          axr = max (fst $ fst $ a) (fst $ snd $ a)
          ayb = min (snd $ fst $ a) (snd $ snd $ a)
          ayt = max (snd $ fst $ a) (snd $ snd $ a)
          bxl = min (fst $ fst $ b) (fst $ snd $ b)
          bxr = max (fst $ fst $ b) (fst $ snd $ b)
          byb = min (snd $ fst $ b) (snd $ snd $ b)
          byt = max (snd $ fst $ b) (snd $ snd $ b)
    

getOrient :: Line -> Orientation
getOrient ((x1,y1),(x2,y2)) = if y1 /= y2 then Vertical else Horizontal

processInstructions :: String -> [Line]
processInstructions s = drop 1 $ reverse (foldl (\lines inst -> (processInstruction inst (snd $ head lines) : lines)) [((0,0),(0,0))] (splitAtCommas s))

processInstruction :: String -> Point -> Line
processInstruction instruction (x,y)
    | direction == 'R' = ((x,y),(x+magnitude,y))
    | direction == 'L' = ((x,y),(x-magnitude,y))
    | direction == 'U' = ((x,y),(x,y+magnitude))
    | direction == 'D' = ((x,y),(x,y-magnitude))
    where direction = head instruction
          magnitude = read $ tail instruction :: Int

splitAtCommas :: String -> [String]
splitAtCommas a = splitRegex (mkRegex "\\,") a 
