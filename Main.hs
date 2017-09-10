
----------------
-- Main module
module Main where

import Control.Monad
import Text.Printf
import Data.List
import System.Random (randomIO)

filePath :: FilePath
filePath = "words.txt"

printedState :: String
printedState = unlines [
    "-------------",
    "             ",
    "             ",
    "             ",
    "             ",
    "             ",
    "-------------"
    ]

data State = State {
  wordToGuess :: String,
  guessCount :: Int,
  guesses :: [Char]
}

getHiddenCharacter :: Char -> String -> Char
getHiddenCharacter character guesses = if (character `elem` guesses)
                                        then character
                                        else '_'


getHiddenWord :: String -> String -> String
getHiddenWord wordToGuess [] = take (length wordToGuess) (repeat '_')
getHiddenWord [] _ = []
getHiddenWord (x:xs) guesses = [getHiddenCharacter x guesses] ++ getHiddenWord xs guesses

findMatch :: State -> Char -> State
findMatch state newChar =
    if (newChar `elem` (wordToGuess state))
    then
        state
    else
        state { guessCount = (guessCount state) + 1 }

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
    putStrLn printedState
    let hiddenWord = getHiddenWord (wordToGuess gameState) (guesses gameState)
    putStrLn "**** ARVATTAVA SANA ****"
    putStrLn ("        " ++ hiddenWord ++ "\n")
    newChar <- getNewCharacter (guesses gameState)
    let newState = findMatch gameState newChar
    gameLoop gameState { guessCount = guessCount newState, guesses = guesses gameState ++ [newChar] }

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
