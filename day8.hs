import Data.List
import Data.Ord

main = do
    day8_1
    day8_2

day8_1 = do
    input <- readFile "input/day8.txt"
    let width = 25
    let height = 6
    let layerSize = width * height
    let noLayers = div (length input) layerSize
    let layers = [take layerSize (drop (n*layerSize) input) | n <- [0..noLayers-1]]
    let myLayer = fst $ minimumBy (comparing snd) [(layer,length $ filter (=='0') layer) | layer <- layers]
    putStrLn $ show $ ((instanceCt '1' myLayer) * (instanceCt '2' myLayer))

day8_2 = do
    input <- readFile "day8.txt"
    let pixels = [read [i] :: Int | i <- input]
    let width = 25
    let height = 6
    let layerSize = width * height
    let noLayers = div (length input) layerSize
    let layers = [zip [0..] (take layerSize (drop (n*layerSize) pixels)) | n <- [0..noLayers-1]]
    
    let image = procLayer layers (zip [0..] (take layerSize (repeat 2)))
    let ilines = [map (\x -> (if x==0 then ' ' else 'o')) (map snd (take width (drop (n*width) image))) | n <- [0..height-1]]
    putStrLn $ show $ (ilines!!0)
    putStrLn $ show $ (ilines!!1)
    putStrLn $ show $ (ilines!!2)
    putStrLn $ show $ (ilines!!3)
    putStrLn $ show $ (ilines!!4)
    putStrLn $ show $ (ilines!!5)
        
instanceCt :: Eq a => a -> [a] -> Int
instanceCt a l = length $ filter (==a) l

procLayer :: [[(Int,Int)]] -> [(Int,Int)] -> [(Int,Int)]
procLayer [] wi = wi
procLayer (layer:layers) wi = procLayer layers [(a,if snd (wi!!a)==2 then snd (layer!!a) else snd (wi!!a)) | a <- [0..length layer-1]]
