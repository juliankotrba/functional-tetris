module Main where 

import GameLogic
import Game 
import Output
import System.IO
import System.Timeout
import System.Console.ANSI

main = do 
    hSetBuffering stdout (BlockBuffering (Just 10)) 
    hSetEcho stdout False
    gameLoop newGame

gameLoop :: TetrisGame -> IO ()
gameLoop game = do
  maybeInputChar <- timeout 1000000 getChar
  updatedGame <- runTurn game { move = inputToMove maybeInputChar}
  putStr $ drawGame updatedGame
  cursorUpLine $ (boardHeight updatedGame) + 2
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