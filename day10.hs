type Point = (Int,Int)
type Line (Point,Point)

main = do
    day10_1
    --day10_2
    
day10_1 = do
    input <- readFile "input/day10.txt"
    let w = 36
    let asteroids =  map fst $ filter (\x -> snd x == '#') (map (\(a,b) -> ((mod a w,div a w),b)) (zip [0..] (filter (/= '\n') input)))
    putStrLn $ show asteroids
