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
, updateHorizontalCountVector
, updateVerticalCountVector
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
type VerticalFullCount = V.Vector Int

initFullCountVector :: Int -> V.Vector Int
initFullCountVector s = V.fromList $ take s $ repeat 0


data TetrisGame = TetrisGame
  { tetromino :: Maybe Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Move
  , horizontalCount :: HorizontalFullCount
  , verticalCount :: VerticalFullCount
  } deriving (Show)

-- Helper functions

newGame = TetrisGame (Just defaultT) initBoard (mkStdGen 0) NoMove (initFullCountVector 12) (initFullCountVector 10) -- TODO: Start with random tetromino

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
        b = board g
        boardHeight = (snd $ bounds b) + 1
        verticalCountVector = verticalCount g 
    in any (\i -> i > boardHeight) verticalCountVector   

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

updateHorizontalCountVector :: HorizontalFullCount -> Positions -> HorizontalFullCount
updateHorizontalCountVector v ps = 
    let 
        ys = map (\y -> (y,1)) $ sort $ map snd ps
        -- TODO: simplify
        grouped = map (\(y,val)->(y, (v V.! y)+val)) $ -- calculate new value in vector
                    map (\y -> foldr (\(x1,y1) (x2,y2) -> (x1, y1+y2)) (0,0) y) $  -- add up groupings
                        groupBy (\a b -> fst a == fst b) ys -- group by x values (e.g. horizontal I tetromino)
    in v V.// grouped

updateVerticalCountVector :: HorizontalFullCount -> Positions -> HorizontalFullCount
updateVerticalCountVector v ps = 
    let 
        xs = map (\x -> (x,1)) $ sort $ map fst ps
        -- TODO: simplify
        grouped = map (\(x,val)->(x, (v V.! x)+val)) $ -- calculate new value in vector
                    map (\x -> foldr (\(x1,y1) (x2,y2) -> (x1, y1+y2)) (0,0) x) $  -- add up groupings
                        groupBy (\a b -> fst a == fst b) xs -- group by x values (e.g. horizontal I tetromino)
    in v V.// grouped