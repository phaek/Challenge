module Nim where

import Data.List          (minimumBy)
import Data.Ord           (comparing)
import System.Environment (getArgs)

type Field = (Int, Int, Int)

initialField :: Field
initialField = (1, 3, 5)

remainingItems :: Field -> Int
remainingItems (a, b, c) = a + b + c

moves :: Field -> [Field]
moves (a, b, c) = [ (a', b, c) | a' <- [a - 1, a - 2..0] ]
               ++ [ (a, b', c) | b' <- [b - 1, b - 2..0] ]
               ++ [ (a, b, c') | c' <- [c - 1, c - 2..0] ]

data Tree a = Node a [Tree a]
            deriving Show

gameTree :: Field -> Tree Field
gameTree f = Node f (map gameTree $ moves f)

leaves :: Tree a -> Int
leaves (Node a b)
  | (length b > 0) = sum (map leaves b)
  | otherwise      = 1

data Player = Hero | Enemy
            deriving (Show, Read)


nextPlayer :: Player -> Player
nextPlayer Hero = Enemy
nextPlayer Enemy = Hero

minimaxScore :: Player -> Tree Field -> Int
minimaxScore Hero  (Node (0,0,0) []) =  1
minimaxScore Enemy (Node (0,0,0) []) = -1
minimaxScore p     (Node _       cs) =
  minOrMax (map (minimaxScore (nextPlayer p)) cs)
    where
      minOrMax = case p of
        Hero -> maximum'
        Enemy -> minimum'

makeMove :: Field -> Field
makeMove = minimumBy (comparing (minimaxScore Hero . gameTree)) . moves

minimum' :: [Int] -> Int
minimum' x = if (elem (-1) x) then -1 else 1

maximum' :: [Int] -> Int
maximum' x = if (elem (1) x) then 1 else -1

-- Commandline interface for playing Nim against our AI.
-- invoked with `stack runhaskell Nim -- Hero 1 3 5` for example.

type Command = (Int, Int)

parseCommand :: String -> Command
parseCommand xs = case words xs of
  [stack, items] -> (read stack, read items)

validateCommand :: Field -> Command -> Bool
validateCommand (a, _, _) (1,stack) = 0 < stack && stack <= a
validateCommand (_, a, _) (2,stack) = 0 < stack && stack <= a
validateCommand (_, _, a) (3,stack) = 0 < stack && stack <= a
validateCommand        _         _  = False

applyCommand :: Command -> Field -> Field
applyCommand (1,n) (a, b, c) = (a - n, b, c)
applyCommand (2,n) (a, b, c) = (a, b - n, c)
applyCommand (3,n) (a, b, c) = (a, b, c - n)

isGameDone :: Field -> Bool
isGameDone = (== (0,0,0))

printResult :: Player -> IO ()
printResult Hero = putStrLn "Congratulations, you won!"
printResult Enemy = putStrLn "You lost!"

performMove :: Player -> Field -> IO Field
performMove Enemy field = return $ makeMove field
performMove Hero field = do
  putStrLn "Enter desired move as: <stack> <items>"
  cmd <- parseCommand <$> getLine
  if validateCommand field cmd
  then return $ applyCommand cmd field
  else performMove Hero field

gameLoop :: Player -> Field -> IO ()
gameLoop player field = 
  if isGameDone field
  then printResult player
  else do
    putStr "Turn:" >> print player
    print field
    gameLoop (nextPlayer player) =<< performMove player field

main :: IO ()
main = do
  [starter,a,b,c] <- getArgs
  gameLoop (read starter) (read a, read b, read c)
