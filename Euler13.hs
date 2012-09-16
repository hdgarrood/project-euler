euler13 :: String -> String
euler13 = (take 10 . show . sum . map read . lines)

main :: IO ()
main = do
    input <- readFile "numbers.txt"
    putStrLn (euler13 input)
