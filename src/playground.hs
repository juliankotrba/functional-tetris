{- TODOs:

Better data structure for tetromino. eg records with type and positions because every tetromino has positions

-}

import Control.Monad.State
import System.Random
import Data.List
import Data.Array
import Data.List.Split
import Data.Maybe
import System.IO
import System.Timeout

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

-- default tetrominos
-- the rotation point of each tetromino is placed at index 0
defaultI = Tetromino I [(1,0), (0,0), (2,0), (3,0)]
defaultO = Tetromino O [(0,0), (1,0), (0,1), (1,1)]
defaultT = Tetromino T [(1,1), (1,0), (0,1), (2,1)]
defaultJ = Tetromino J [(1,1), (2,0), (0,1), (2,1)]
defaultL = Tetromino L [(1,1), (0,0), (0,1), (2,1)]
defaultS = Tetromino S [(1,1), (1,0), (2,0), (0,1)]
defaultZ = Tetromino Z [(1,1), (0,0), (1,0), (2,1)]

data Move = RotateR | MoveLeft | MoveRight | MoveDown deriving (Show)

data TetrisGame = TetrisGame
  { tetromino :: Tetromino
  , board :: Board 
  , tGenerator :: StdGen
  , move :: Maybe Move
  } deriving (Show)

newGame = TetrisGame defaultT initBoard (mkStdGen 0) Nothing

spawnTetromino :: State TetrisGame ()
spawnTetromino = do
        currentState <- get
        let (rn, gen') = randomR (0,6) $ tGenerator currentState
        put (currentState { tGenerator = gen', tetromino = toEnum rn } )
        return ()
        
moveTetromino :: StateT TetrisGame IO ()
moveTetromino = do
        currentState <- get
        case (move currentState) of
          Nothing -> return ()
          Just MoveLeft -> put (currentState { tetromino = left $ tetromino currentState, move = Nothing } )
          Just MoveRight -> put (currentState { tetromino = right $ tetromino currentState, move = Nothing } )
          Just RotateR -> put (currentState { tetromino = rotateR $ tetromino currentState, move = Nothing } )
          Just MoveDown -> put (currentState { tetromino = down $ tetromino currentState, move = Nothing } )

resolveTurn :: StateT TetrisGame IO ()  
resolveTurn = do
    currentState <- get
    moveTetromino
    return ()

runTurn = execStateT resolveTurn-- newBoard

-- tetrimino helpers

pivot :: TetrominoType -> Int
pivot t = case t of
    I -> 4
    O -> 2
    T -> 3
    J -> 3
    L -> 3
    S -> 3
    Z -> 3

type RotationPoint = Position

rightRotation :: Position -> RotationPoint -> Position
rightRotation (x,y) (rx, ry) = 
  let 
    nx = (x - rx) * (-1) -- normalized x
    ny = (y - ry) * (-1) -- normalized y
  in ((-ny*(-1))+rx, (nx*(-1))+ry)

rotateR :: Tetromino -> Tetromino
rotateR t = 
  let
    ps = positions t
    rotationPoint = ps !! 0 -- the first element is the rotation point
  in t { positions = map (\p -> rightRotation p rotationPoint) ps }

right :: Tetromino -> Tetromino -- TODO: Check bounds, wall kicks
right tetromino = tetromino { positions = map (\(x,y) -> (x+1,y)) $ positions tetromino }

left :: Tetromino -> Tetromino -- TODO: Check bounds, wall kicks
left tetromino = tetromino { positions = map (\(x,y) -> (x-1,y)) $ positions tetromino }

down :: Tetromino -> Tetromino
down tetromino = 
  let
    postions = positions tetromino
  in 
    tetromino { positions = map (\(x,y) -> (x, y+1)) postions}

-- output helpers

lineToString :: [FieldState] -> String
lineToString fss = intercalate "" $ map (\fs -> if fs == FULL then "X" else ".") fss

drawGame :: TetrisGame -> String
drawGame game = let
  t = tetromino game
  b = drawTetrominoToBoard (board game) t
  lineSize = length $ elems $ b!0
  list2d = chunksOf lineSize $ concat $ map elems $ elems b
  in (unlines $ map lineToString list2d) ++ "\n"

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
  gameLoop newGame

gameLoop :: TetrisGame -> IO ()
gameLoop game = do
  inputChar <- getChar
  newGame <- runTurn game { move = charToMove inputChar}
  putStr $ drawGame newGame
  gameLoop newGame

charToMove :: Char -> Maybe Move
charToMove 'w' = Just RotateR
charToMove 'a' = Just MoveLeft
charToMove 's' = Just MoveDown
charToMove 'd' = Just MoveRight
charToMove _ = Nothing