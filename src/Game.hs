module Game 
( Board (..)
, FieldState (..)
, Move (..)
, TetrisGame (..)
, initBoard
, newGame
, anyFull
, anyBeyondBottom
, isOutOfBounds
, boardHeight
) where

import Tetromino
import System.Random
import Data.Array

type Board = Array Int (Array Int FieldState)

data FieldState = EMPTY | FULL deriving (Eq, Show)

data Move = RotateR | MoveLeft | MoveRight | MoveDown | NoMove deriving (Show)

data TetrisGame = TetrisGame
  { tetromino :: Maybe Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Move
  } deriving (Show)

-- Helper functions

newGame = TetrisGame (Just defaultT) initBoard (mkStdGen 0) NoMove -- TODO: Start with random tetromino

initBoard = array (0,11) 
  [ (0, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (1, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (2, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (3, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (4, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (5, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (6, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (7, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (8, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (9, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (10, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  , (11, array (0,9) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY), (5,EMPTY), (6,EMPTY), (7,EMPTY), (8,EMPTY), (9,EMPTY)])
  ] :: Board -- TODO: Simplify with list comprehension  


anyBeyondBottom :: Board -> Positions -> Bool
anyBeyondBottom b ps = 
    let
        bottomBound = snd $ bounds b
        ys = map snd ps
    in any (> bottomBound) ys       

-- Checks if any of the passed positions is horizontally out of bounds 
isOutOfBounds :: Board -> Positions -> Bool
isOutOfBounds b ps = 
    let
        rightBound = snd $ bounds $ b ! 0 
        xs = map fst ps
    in any (\x -> x<0 || x > rightBound) xs 
  
-- Checks if any of the passed positions is marked as FULL on the board   
anyFull :: Board -> Positions -> Bool
anyFull board ps = any isAnyFull ps where
    isAnyFull p = isFull board p          

-- Checks if the passed position is marked as FULL on the Board    
isFull :: Board -> Position -> Bool
isFull board (x,y) = board ! y ! x == FULL  

boardHeight :: TetrisGame -> Int
boardHeight g = snd $ bounds $ board g