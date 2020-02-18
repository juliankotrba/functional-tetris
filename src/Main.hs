module Main where 

import GameLogic
import Game 
import System.IO
import System.Timeout

main = do gameLoop newGame

gameLoop :: TetrisGame -> IO ()
gameLoop game = do
  maybeInputChar <- timeout 1000000 getChar
  updatedGame <- runTurn game { move = inputToMove maybeInputChar}
  putStr $ drawGame updatedGame
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