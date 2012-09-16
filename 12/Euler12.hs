divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

nthTriangle :: Int -> Int
nthTriangle n = sum [1..n]

divisors :: Int -> [Int]
divisors n = filter (divides n) [1..(floor $ sqrt n)]
