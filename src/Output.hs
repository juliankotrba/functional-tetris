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
  mt = tetromino game
  b = case mt of 
    Just t -> addTetrominoToBoard (board game) t
    Nothing -> board game
  lineSize = length $ elems $ b!0
  list2d = chunksOf lineSize $ concat $ map elems $ elems b
  in (unlines $ map lineToString list2d) ++ "\n"

sideWall = [chr 0x2502]

lineToString :: [FieldState] -> String
lineToString fss = sideWall ++ (intercalate "" $ map (\fs -> if fs == FULL then [chr 0x2588] else " ") fss) ++ sideWall