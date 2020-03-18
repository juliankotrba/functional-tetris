module Main where 

import GameLogic
import Game 
import Output
import System.IO
import System.Timeout
import System.Console.ANSI

start = main

main = do 
    setTitle "3 2 1 Tetris!"
    hSetBuffering stdout (BlockBuffering (Just 10)) 
    hSetEcho stdout False
    gameLoop newGame

gameLoop :: TetrisGame -> IO ()
gameLoop game = do
  maybeInputChar <- timeout 500000 getChar
  updatedGame <- runTurn game { move = inputToMove maybeInputChar}
  putStr $ drawGame updatedGame
  cursorUpLine $ (boardHeight $ board updatedGame) + 3
  gameLoop updatedGame

inputToMove :: Maybe Char -> Move
inputToMove mi = case mi of
    Just i -> charToMove i
    Nothing -> charToMove 's'

charToMove :: Char -> Move
charToMove 'w' = RotateR
charToMove 'a' = MoveLeft
charToMove 's' = MoveDown
charToMove 'd' = MoveRight
charToMove _ = NoMove