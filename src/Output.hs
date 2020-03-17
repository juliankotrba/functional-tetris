module Output
( drawGame
) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Array
import GameLogic
import Game

drawGame :: TetrisGame -> String
drawGame game = let
  maybeTetromino = tetromino game
  b = case maybeTetromino of 
    Just t -> addTetrominoToBoard (board game) t 
    Nothing -> board game
  lineSize = length $ elems $ b!0
  list2d = chunksOf lineSize $ concat $ map elems $ elems b
  width = boardWidth game
  in (scoreLine 2) ++
      topLeftCorner ++ (horizontalFieldSeparator width) ++ topRightCorner ++"\n" ++ 
        (unlines $ map lineToString list2d) ++
          bottomLeftCorner ++ (horizontalFieldSeparator width) ++ bottomRightCorner

sideWall = [chr 0x2502]
horizontalLine = [chr 0x2500]
bottomLeftCorner = [chr 0x2514]
bottomRightCorner = [chr 0x2518]
topRightCorner = [chr 0x2510]
topLeftCorner = [chr 0x250c]

scoreLine :: Int -> String
scoreLine score = "Score: " ++ (show score) ++ "\n"

horizontalFieldSeparator :: Int -> String
horizontalFieldSeparator boardWidth = concat $ replicate boardWidth horizontalLine

lineToString :: [FieldState] -> String
lineToString fss = sideWall ++ (intercalate "" $ map (\fs -> if fs == FULL then [chr 0x2588] else " ") fss) ++ sideWall