collatz :: Int -> [Int]
collatz n
    | n == 1    =   [1]
    | even n    =   n : collatz (n `div` 2)
    | otherwise =   n : collatz (3*n + 1)

euler14 :: Int
euler14 = let starts = [1,2..999999]
              results = map (length . collatz) starts
          in maximum results 

main :: IO ()
main = putStrLn (show euler14)
