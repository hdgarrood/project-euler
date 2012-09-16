module Euler391 where

import Data.Tree
import Data.List

data Turn = One | Two deriving (Show, Eq)

next One = Two
next Two = One

differences :: [Integer]
differences = 1 : (differences >>= (\x -> [x,x+1]))

sk :: [Integer]
sk = scanl (+) 0 differences

elem_sk :: Integer -> Bool
elem_sk n = n == last (takeWhile (<= n) sk)

validMoves :: Integer -> Integer -> [Integer]
validMoves n c = filter elem_sk (map (+c) [1..n])

buildGameTree :: Integer -> Integer -> Tree Integer
buildGameTree n c = unfoldTree (\x -> (x, validMoves n x)) c

-- takes a tree and the current depth, returns a list of depths of all
-- leaves in the given tree
depthOfLeaves :: Integer -> Tree a -> [Integer]
depthOfLeaves currentDepth tree
    | null sub  = [currentDepth]
    | otherwise = sub >>= depthOfLeaves (currentDepth + 1)
    where sub = subForest tree

-- just for convenience; prIntegers a graphical (text) representation of a
-- tree
putGameTree :: Tree Integer -> IO ()
putGameTree = putStr . drawTree . fmap show

-- given a game with parameter n, and that the counter's value is
-- currently c, and whose turn it is, can player one always win?
playerOneWins :: Integer -> Turn -> Integer -> Bool
playerOneWins n turn c = let moves = validMoves n c
                             anyOrAll = if turn == One
                                            then any
                                            else all
                             nextTurn = next turn
                         in if null moves
                                then turn == Two
                                else anyOrAll (playerOneWins n nextTurn) moves

funcM :: Integer -> Integer
funcM n = let startingMoves         = validMoves n 0
              highestWinningMove    = (take 1 . filter (playerOneWins n Two) . reverse) $ startingMoves
          in case highestWinningMove of
              []  ->  0
              [x] ->  x

iterMoves :: Integer -> ([Integer], Turn) -> ([Integer], Turn)
iterMoves n (cs, turn) = let nextCs = nub $ concatMap (validMoves n) cs
                             nextTurn = next turn
                          in (nextCs, nextTurn)

-- given the value of n for a game, what value will c have when the game
-- ends?
finalC :: Integer -> Integer
finalC n = sk !! (2 * ((2 ^ n) - 1))
