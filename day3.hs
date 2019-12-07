import Text.Regex
import Data.List

type Point = (Int,Int)
type Line = (Point,Point)
data Orientation = Horizontal | Vertical deriving (Eq)

main = do
    day3_1
    --day3_2

day3_1 = do
    input <- readFile "input/day3.txt"
    putStrLn ("Day 03, Problem 1: " ++ (show $ minimum $ map manhattan (getIntersects (sortLines $ processInstructions $ head $ lines input) (sortLines $ processInstructions $ last $ lines input))))

day3_2 = do
    input <- readFile "input/day3.txt"
    putStrLn ("Day 03, Problem 2: " ++ (show $ stepsToPoint (sortLines $ processInstructions "R8,U5,L5,D3") (3,3)))
    

manhattan :: Point -> Int
manhattan p = (abs $ fst p) + (abs $ snd p)

stepsToPoint :: [Line] -> Point -> Int
stepsToPoint (l:ls) p = if pointOnLine l p then lineLength l else (lineLength l) + (stepsToPoint ls p)



lineLength :: Line -> Int
lineLength ((xl,yb),(xr,yt)) = if getOrient ((xl,yb),(xr,yt)) == Vertical then yt-yb else xr-xl

pointOnLine :: Line -> Point -> Bool
pointOnLine ((xl,yb),(xr,yt)) (x,y)
    | getOrient ((xl,yb),(xr,yt)) == Vertical = xl == x && yb <= y && y <= yt
    | otherwise = yb == y && xl <= x && x <= xr

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
    where axl = fst $ fst $ a
          axr = fst $ snd $ a
          ayb = snd $ fst $ a
          ayt = snd $ snd $ a
          bxl = fst $ fst $ b
          bxr = fst $ snd $ b
          byb = snd $ fst $ b
          byt = snd $ snd $ b
    

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
