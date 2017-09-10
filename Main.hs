
----------------
-- Main module
module Main where

import Control.Monad
import Text.Printf
import Data.List
import System.Random (randomIO)

filePath :: FilePath
filePath = "words.txt"


data State = State {
  wordToGuess :: String,
  guessCount :: Int,
  guesses :: [Char]
}

getCharacter :: IO Char
getCharacter = do
    line <- getLine
    case line of
        [] -> getCharacter
        (c:_) -> return c

getNewCharacter :: [Char] -> IO Char
getNewCharacter guesses = do
    putStrLn "**** SYÖTÄ KIRJAIN ***"
    c <- getCharacter
    if (c `elem` guesses)
    then do
        putStrLn "Merkkiä on haettu jo"
        getNewCharacter guesses
    else
        return c

gameLoop :: State -> IO()
gameLoop gameState = do
    putStrLn ("Etsitty sana on = " ++ (wordToGuess gameState) ++ " ja vääriä arvauksia " ++ show (guessCount gameState))
    putStrLn ("Syötä uusi kirjain")
    c <- getChar
    putStrLn ("Syötetty kirjain oli " ++ [c])
    newChar <- getNewCharacter (guesses gameState)

newGame :: String -> IO()
newGame filename = do
    contents <- readFile filename
    let words' = lines contents
    let wordCount = length words'
    randomNumber <- randomIO
    let randomWord = words' !! (randomNumber `mod` wordCount)
    putStrLn ("Haettu sana " ++ randomWord)
    gameLoop (State randomWord 0 [])

loop = do
    putStrLn "**** UUSI PELI ****"
    putStrLn "*******************"
    newGame filePath

main = loop
