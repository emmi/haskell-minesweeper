
----------------
-- Main module
module Main where

import Control.Monad
import Data.List

data Point = Point (Int, Int) deriving (Show, Eq)

data Board = Board {
  points :: [[(Int, Int)]]
} deriving (Show, Eq)

data State = State {
  board :: Board
}


generateNewBoard :: State -> State
generateNewBoard state =
  let locations = map (\x -> (map (\y -> (x,y)) [0,1..9])) [0,1..9]
  in state { board = Board locations }

printBoard :: Board -> IO()
printBoard board =
  let characters = foldl (\acc row -> acc ++ (foldl (\acc_ point -> acc_ ++ printCharacter point) acc row)) [] (points board)
  in putStr characters

printCharacter :: (Int, Int) -> String
printCharacter point
 | snd point == 9 = " *\n"
 | fst point == 0 || fst point == 9 = " * "
 | snd point == 0  = " * "
 | otherwise = "   "

loop state = do
  line <- getLine
  case line of
    "new" -> do
      putStrLn "Luodaan uusi kenttä"
      let updatedState = generateNewBoard state
      print (board updatedState)
      loop updatedState
    "print" -> do
      putStrLn "Tulostetaan kenttä"
      printBoard (board state)
      putStrLn "tulostettu"
      loop state
    "guess" -> do
      putStrLn "Siirretään"
      loop state
    "quit" -> do
      putStrLn "Lopetetaan"
    _ -> do
      putStrLn "Tuntematon komento"
      loop state

main = loop (State (Board []))




---
--   x  x  x  x  x  x  x  x  x  x  x  x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  1  2  3  4  5  6  7  8  9  10 x
--   x  x  x  x  x  x  x  x  x  x  x  x

----
