import System.IO
import Data.Maybe

-- grid stuff
readGridLine :: String -> [Int]
readGridLine line = map read (words line)

readGrid :: String -> [[Int]]
readGrid grid = map readGridLine (lines grid)

-- vector stuff
data Vector = Vector Int Int deriving (Show)

vfst :: Vector -> Int
vfst (Vector x _) = x

vsnd :: Vector -> Int
vsnd (Vector _ x) = x

vplus :: Vector -> Vector -> Vector
vplus (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

vflip :: Vector -> Vector
vflip (Vector x y) = Vector (x) (-y)

horizontal = [Vector 0 0, Vector 1 0, Vector 2 0, Vector 3 0]
vertical = [Vector 0 0, Vector 0 1, Vector 0 2, Vector 0 3]
diagonal1 = zipWith (vplus) horizontal vertical
diagonal2 = map vflip diagonal1
directions = [horizontal, vertical, diagonal1, diagonal2]

adjProductInRange :: [[Int]] -> [Vector] -> Vector -> Bool
adjProductInRange grid direction start =
    let coords = map (vplus start) direction
        width = length $ grid !! 0
        height = length grid
    in (maximum (map vfst coords) <= (width - 1)) && (maximum (map vsnd coords) <= (height - 1))
        && (minimum (map vfst coords) >= 0) && (minimum (map vsnd coords) >= 0)

adjProduct :: [[Int]] -> [Vector] -> Vector -> Int
adjProduct grid direction start =
    let coords = map (vplus start) direction
        values = map (valueAt grid) coords                               
    in product values                                                    

maybeAdjProduct :: [[Int]] -> [Vector] -> Vector -> Maybe Int
maybeAdjProduct grid direction start =
    if adjProductInRange grid direction start
        then Just $ adjProduct grid direction start    
        else Nothing

valueAt :: [[Int]] -> Vector -> Int
valueAt grid v = grid !! vsnd v !! vfst v

euler11 :: String -> Int
euler11 gridStr = let grid = readGrid gridStr
                      width = length $ grid !! 0
                      height = length grid
                      allStarts = [Vector x y | x <- [0..width-1], y <- [0..height-1]]
                      maybeProductsInDirection = (\x -> map (maybeAdjProduct grid x) allStarts)
                      productsInDirection = catMaybes . maybeProductsInDirection
                      allProducts = concat $ map productsInDirection directions
                  in maximum allProducts

main = do
    gridStr <- readFile "grid.txt"
    putStrLn $ show (euler11 gridStr)
