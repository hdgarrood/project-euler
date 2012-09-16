import Euler391 (funcM)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let n = (read . head) $ args
        result = (show . funcM) $ n
    putStrLn result
