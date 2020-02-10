{- TODOs:

Better data structure for tetromino. eg records with type and positions because every tetromino has positions

-}

import Control.Monad.State
import System.Random
import Data.List
import Data.Array
import Data.List.Split
import Data.Maybe

data FieldState = EMPTY | FULL deriving (Eq, Show)

type Board = Array Int (Array Int FieldState) --[[FieldState]]

initBoard = array (0,11) [ (0, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (1, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (2, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (3, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (4, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (5, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (6, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (7, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (8, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (9, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (10, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  , (11, array (0,4) [(0,EMPTY), (1,EMPTY), (2,EMPTY), (3,EMPTY), (4,EMPTY)])
  ] :: Board
--[ [ EMPTY | j <- [1..10] ] | i <- [1..20] ]

type Position = (Int, Int)
type Positions = [Position]

data TetrominoType = I | O| T | J | L | S | Z deriving (Show, Eq, Ord)

data Tetromino = Tetromino 
  { tetrominoType :: TetrominoType
  , positions :: Positions 
  } deriving (Show, Eq) 

instance Enum Tetromino where
  toEnum 0 = defaultI
  toEnum 1 = defaultO
  toEnum 2 = defaultT
  toEnum 3 = defaultJ
  toEnum 4 = defaultL
  toEnum 5 = defaultS
  toEnum 6 = defaultZ

  fromEnum _ = 0 -- TODO

defaultI = Tetromino I [(0,0), (1,0), (2,0), (3,0)]
defaultO = Tetromino O [(0,0), (1,0), (0,1), (1,1)]
defaultT = Tetromino T [(1,0), (0,1), (1,1), (2,1)]
defaultJ = Tetromino J [(2,0), (0,1), (1,1), (2,1)]
defaultL = Tetromino L [(0,0), (0,1), (1,1), (2,1)]
defaultS = Tetromino S [(1,0), (2,0), (0,1), (1,1)]
defaultZ = Tetromino Z [(0,0), (1,0), (1,1), (2,1)]

data Move = RotateR | RotateL | MoveLeft | MoveRight deriving (Show)

data TetrisGame = TetrisGame
  { tetromino :: Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Maybe Move
  } deriving (Show)

newBoard = TetrisGame defaultI initBoard (mkStdGen 0) Nothing

spawnTetromino :: State TetrisGame ()
spawnTetromino = do
        currentState <- get
        let (rn, gen') = randomR (0,6) $ tGenerator currentState
        put (currentState { tGenerator = gen', tetromino = toEnum rn } )
        return ()

moveTetrominoDown :: State TetrisGame ()
moveTetrominoDown = do
        currentState <- get
        put (currentState { tetromino = down (tetromino currentState)} )
        return () 
        
moveTetromino :: State TetrisGame ()
moveTetromino = do
        currentState <- get
        case (move currentState) of
          Nothing -> return ()
          Just MoveLeft   -> return ()
          Just MoveRight  -> return ()
          Just RotateR    -> return ()
          Just RotateL    -> return ()

resolveTurn :: State TetrisGame ()  
resolveTurn = do
    currentState <- get
    moveTetrominoDown
    return ()

run = runState resolveTurn -- newBoard

-- tetrimino helpers

-- https://keisan.casio.com/exec/system/1223522781
rightRotation :: Position -> Position
rightRotation (x,y) = (y*1, -x)

leftRotation :: Position -> Position
leftRotation (x,y) = (-1*y, x)

down :: Tetromino -> Tetromino
down tetromino = 
  let
    postions = positions tetromino
  in 
    tetromino { positions = map (\(x,y) -> (x, y+1)) postions}

-- output helpers

lineToString :: [FieldState] -> String
lineToString fss = intercalate "" $ map (\fs -> if fs == FULL then "X" else ".") fss

boardToString :: Board -> String
boardToString b = let
  lineSize = length $ elems $ b!0
  list2d = chunksOf lineSize $ concat $ map elems $ elems b 
  in unlines $ map lineToString list2d

drawTetrominoToBoard :: Board -> Tetromino -> Board
drawTetrominoToBoard b t = drawTetrominoToBoardHelper b $ positions t 

drawTetrominoToBoardHelper :: Board -> Positions -> Board
drawTetrominoToBoardHelper b [] = b
drawTetrominoToBoardHelper b ((x,y):xs) = let
  yRow = b ! y
  updatedRow = yRow // [(x, FULL)]
  updatedBoard = b // [(y, updatedRow)]
  in drawTetrominoToBoardHelper updatedBoard xs 

-- main

main = do
  putStr "Hello World"
  print $ snd $ run newBoard
  putStr "Hello World"