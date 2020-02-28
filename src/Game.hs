module Game 
( Board (..)
, FieldState (..)
, Move (..)
, TetrisGame (..)
, initBoard
, newGame
, isGameOver
, anyFull
, anyBeyondBottom
, isOutOfBounds
, boardHeight
, boardWidth
, updateHorizontalCountVector
) where

import Tetromino
import System.Random
import Data.Array
import Data.List (sortBy, groupBy, sort)
import Data.Function (on)
import qualified Data.Vector as V

type Board = Array Int (Array Int FieldState)

data FieldState = EMPTY | FULL deriving (Eq, Show)

data Move = RotateR | MoveLeft | MoveRight | MoveDown | NoMove deriving (Show)

type HorizontalFullCount = V.Vector Int

initHorizontalCountVector :: Int -> V.Vector Int
initHorizontalCountVector s = V.fromList $ take s $ repeat 0

data TetrisGame = TetrisGame
  { tetromino :: Maybe Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Move
  , horizontalCount :: HorizontalFullCount
  } deriving (Show)

-- Helper functions

newGame = TetrisGame (Just defaultT) initBoard (mkStdGen 0) NoMove (initHorizontalCountVector 12) -- TODO: Start with random tetromino

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

isGameOver :: TetrisGame -> Bool
isGameOver g = 
    let
        mTetromino = tetromino g
        ys = case mTetromino of
            Nothing -> []
            Just t -> map snd $ positions t
    in any (<0) ys   

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
        bottomBound = snd $ bounds b
        xs = map fst ps
        ys = map snd ps
    in 
        any (\x -> x<0 || x > rightBound) xs || 
        any (\y-> y>bottomBound || y<0) ys
  
-- Checks if any of the passed positions is marked as FULL on the board   
anyFull :: Board -> Positions -> Bool
anyFull board ps = any isAnyFull ps where
    isAnyFull p = isFull board p          

-- Checks if the passed position is marked as FULL on the Board    
isFull :: Board -> Position -> Bool
isFull board (x,y) = board ! y ! x == FULL  

boardHeight :: TetrisGame -> Int
boardHeight g = snd $ bounds $ board g

boardWidth :: TetrisGame -> Int
boardWidth g = 1 + (snd $ bounds $ (board g) ! 0)

updateHorizontalCountVector :: HorizontalFullCount -> Positions -> HorizontalFullCount
updateHorizontalCountVector v ps = 
    let 
        ys = map (\y -> (y,1)) $ sort $ map snd ps
        -- TODO: simplify
        grouped = map (\(y,val)->(y, (v V.! y)+val)) $ -- calculate new value in vector
                    map (\y -> foldr (\(x1,y1) (x2,y2) -> (x1, y1+y2)) (0,0) y) $  -- add up groupings
                        groupBy (\a b -> fst a == fst b) ys -- group by x values (e.g. horizontal I tetromino)
    in v V.// grouped