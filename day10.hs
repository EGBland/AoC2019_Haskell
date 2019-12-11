import Data.List
import Data.Ord

type Point = (Int,Int)
type Line = (Point,Point)
type Pace = (Int,Int)

type Asteroid = Point
type Asteroids = [Asteroid]
main = do
    day10_1
    day10_2
    
day10_1 = do
    input <- readFile "input/day10.txt"
    let w = 36
    let asteroids = map fst $ filter (\x -> snd x == '#') (map (\(a,b) -> ((mod a w,div a w),b)) (zip [0..] (filter (/= '\n') input)))
    let paces = (1,0):(0,1):filter coprime [(x,y) | x <- [1..w], y <- [1..w]]
    let fullpaces = nub $ concat [[(x,y),(-x,y),(x,-y),(-x,-y)] | (x,y) <- paces]
    
    let collisions = maximum [length $ nub $ filter (/= Nothing) [collision (pace a p) asteroids | p <- fullpaces] | a <- asteroids]
    putStrLn $ show $ collisions

day10_2 = do
    input <- readFile "input/day10.txt"
    let w = 36
    let asteroids = map fst $ filter (\x -> snd x == '#') (map (\(a,b) -> ((mod a w,div a w),b)) (zip [0..] (filter (/= '\n') input)))
    let paces = sortBy (\(x1,y1) (x2,y2) -> compare ((read (show y2)::Double)/(read (show x2)::Double)) ((read (show y1)::Double)/(read (show x1)::Double))) ((1,0):(0,1):filter coprime [(x,y) | x <- [1..w], y <- [1..w]])
    let fullpaces = nub $ concat [[(x,-y) | (x,y) <- paces], reverse paces, [(-x,y) | (x,y) <- paces], reverse [(-x,-y) | (x,y) <- paces]]
    let bestAsteroid = fst $ maximumBy (comparing snd) [(ast, length $ nub $ filter (/= Nothing) [collision (pace ast p) asteroids | p <- fullpaces]) | ast <- asteroids]
    putStrLn $ show $ head $ filter (\(i,_) -> i==200) $ blastAsteroid bestAsteroid (sub asteroids [bestAsteroid]) fullpaces

blastAsteroid :: Asteroid -> Asteroids -> [Pace] -> [(Int,Maybe Asteroid)]
blastAsteroid origin asteroids paces = blastAsteroid' origin (map Just asteroids) paces paces 1

blastAsteroid' :: Asteroid -> [Maybe Asteroid] -> [Pace] -> [Pace] -> Int -> [(Int,Maybe Asteroid)]
blastAsteroid' _ [] _ _ _ = []
blastAsteroid' origin asteroids paces [] i = blastAsteroid' origin asteroids paces paces i
blastAsteroid' origin asteroids paces (p:ps) i = let col = collision' (pace origin p) asteroids in if col == Nothing then blastAsteroid' origin asteroids paces ps i else (i,col):(blastAsteroid' origin (sub asteroids [col]) paces ps (i+1))

pointPlus :: Point -> Point -> Point
pointPlus (x1,y1) (x2,y2) = (x1+x2,y1+y2)

collision :: [Point] -> Asteroids -> Maybe Asteroid
collision [] _ = Nothing
collision (p:ps) asteroids = if elem p asteroids then Just p else collision ps asteroids

collision' :: [Point] -> [Maybe Asteroid] -> Maybe Asteroid
collision' [] _ = Nothing
collision' (p:ps) asteroids = if elem (Just p) asteroids then Just p else collision' ps asteroids

pace :: Point -> Pace -> [Point]
pace p thePace = let q = pointPlus p thePace in if 0<=(fst q) && (fst q)<36 && 0<=(snd q) && (snd q)<36 then q : pace q thePace else []

coprime :: (Int,Int) -> Bool
coprime (a,b) = let pfs = pf a in sub pfs (pf b) == pfs

sub :: Eq a => [a] -> [a] -> [a]
sub a b = filter (\x -> not $ elem x b) a

pf :: Int -> [Int]
pf 1 = []
pf n
    | null factors = [n]
    | otherwise = factors ++ pf (div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2..n-1]
