
----------------
-- Main module
module Main where

import Control.Monad
import Text.Printf
import Data.List
import System.Random (randomIO)

filePath :: FilePath
filePath = "words.txt"

-- data Point = Point (Int, Int) deriving (Show, Eq)
--
-- data Board = Board {
--   points :: [[(Int, Int)]]
-- } deriving (Show, Eq)

data State = State {
  wordToGuess :: String,
  guessCount :: Int
}


-- generateNewBoard :: State -> State
-- generateNewBoard state =
--   let locations = map (\x -> (map (\y -> (x,y)) [0,1..9])) [0,1,2]
--   in state { board = Board locations }
--
-- -- printBoard :: Board -> IO()
-- printBoard board =
--   -- let characters = foldl (\acc row -> acc ++ (foldl (\acc_ point -> acc_ ++ printCharacter point) acc row)) [] (points board)
--   -- let characters = map (\acc_ row -> printRow row) (points board)
--     --   characters2 = foldl (\acc_ point -> acc_ ++ printCharacter point) [] ()
--   -- let characters = map (map printCharacter) (points board)
--   --     characters2 = foldl (\acc point -> acc ++ point) [] characters
--   --   -- map (map change) $ itrCol xs
--   --
--   --   in printBoard2 characters2
--   -- in putStrLn "Böö"

-- printBoard2 :: [String] -> IO()
-- printBoard2 board = putStrLn board

-- printRow :: [(Int, Int)] -> IO()
-- printRow row =
--   print "tylos"

-- printCharacter :: (Int, Int) -> String
-- printCharacter point
--  | snd point == 9 = " *\n"
--  | fst point == 0 || fst point == 2 = " * "
--  | snd point == 0  = " x "
--  | otherwise = "   "

gameLoop :: State -> IO()
gameLoop gameState = do
    putStrLn ("Etsitty sana on = " ++ (wordToGuess gameState) ++ " ja vääriä arvauksia " ++ show (guessCount gameState))
    putStrLn ("Syötä uusi kirjain")
    c <- getChar
    putStrLn ("Syötetty kirjain oli " ++ [c])

newGame :: String -> IO()
newGame filename = do
    contents <- readFile filename
    let words' = lines contents
    let wordCount = length words'
    randomNumber <- randomIO
    let randomWord = words' !! (randomNumber `mod` wordCount)
    putStrLn ("Haettu sana " ++ randomWord)
    gameLoop (State randomWord 0)

loop = do
  line <- getLine
  case line of
    -- "newBoard" -> do
    --   putStrLn "Luodaan uusi kenttä"
    --   let updatedState = generateNewBoard state
    --       -- board > board updatedState
    --   print (board updatedState)
    --   loop
    -- "print" -> do
    --   putStrLn "Tulostetaan kenttä"
    --   printBoard (board state)
    --   putStrLn "tulostettu"
    --   loop state
    "new" -> do
        putStrLn "Aloitetaan uusi peli"
        newGame filePath
        loop
    "quit" -> do
      putStrLn "Lopetetaan"
    _ -> do
      putStrLn "Tuntematon komento"
      loop

main = loop




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
