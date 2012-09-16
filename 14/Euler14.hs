import Data.Function
import Data.List

collatz :: Int -> [Int]
collatz n
    | n == 1    =   [1]
    | even n    =   n : collatz (n `div` 2)
    | otherwise =   n : collatz (3*n + 1)

euler14 :: (Int, Int)
euler14 = let starts = [1,2..999999]
              lengths = map (length . collatz) starts
              results = zip starts lengths
          in maximumBy (compare `on` snd) results 

main :: IO ()
main = putStrLn (show euler14)
