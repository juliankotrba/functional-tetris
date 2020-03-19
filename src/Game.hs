module Game 
( Board (..)
, FieldState (..)
, Move (..)
, TetrisGame (..)
, Scoring (..)
, initBoard
, newGame
, isGameOver
, anyFull
, anyBeyondBottom
, isOutOfBounds
, boardHeight
, boardWidth
, calculateHorizontalCount
, updateHorizontalCountVector
, calculateScore
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

data Scoring = Scoring { score :: Int } deriving Show-- TODO: Combos, back-to-back, etc.

instance Semigroup Scoring where
    (<>) s1 s2 = Scoring $ (score s1) + (score s2)

initHorizontalCountVector :: Int -> V.Vector Int
initHorizontalCountVector s = V.fromList $ take s $ repeat 0

data TetrisGame = TetrisGame
  { tetromino :: Maybe Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Move
  , horizontalCount :: HorizontalFullCount
  , scoring :: Scoring
  } deriving (Show)

-- Helper functions

initScoring = Scoring 0
newGame = TetrisGame (Just defaultT) initBoard (mkStdGen 0) NoMove (initHorizontalCountVector 12) initScoring -- TODO: Start with random tetromino

defaultWidth = 10
defaultHeight = 15

initBoard = array (0, defaultHeight-1) [ (i, initBoardRow) | i <- [0..(defaultHeight-1)]] :: Board
initBoardRow = array (0, defaultWidth-1) [ (i,EMPTY) | i <- [0..(defaultWidth-1)]]

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

boardHeight :: Board -> Int
boardHeight b = snd $ bounds $ b

boardWidth :: Board -> Int
boardWidth b = 1 + (snd $ bounds $ b ! 0)

calculateHorizontalCount :: Board -> HorizontalFullCount
calculateHorizontalCount b = V.fromList $ map (\a -> length $ filter (==FULL) (elems a)) (elems b)

updateHorizontalCountVector :: HorizontalFullCount -> Positions -> HorizontalFullCount
updateHorizontalCountVector v ps = 
    let 
        ys = map (\y -> (y,1)) $ sort $ map snd ps
        -- TODO: simplify
        grouped = map (\(y,val)->(y, (v V.! y)+val)) $ -- calculate new value in vector
                    map (\y -> foldr (\(x1,y1) (x2,y2) -> (x1, y1+y2)) (0,0) y) $  -- add up groupings
                        groupBy (\a b -> fst a == fst b) ys -- group by x values (e.g. horizontal I tetromino)
    in v V.// grouped

calculateScore :: TetrisGame -> Scoring
calculateScore game = 
    let
        horizontalCount_ = horizontalCount game
        width = boardWidth $ board game
        fullRowCount = V.length $ V.filter (== width) horizontalCount_
    in Scoring (if (fullRowCount == 4) then 800 else (100*fullRowCount))