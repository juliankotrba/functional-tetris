module GameLogic
( runTurn
, drawGame
) where
  
import Control.Monad.State
import System.Random
import Data.Array
import Data.List
import Data.List.Split
import Tetromino
import Game

runTurn = execStateT resolveTurn

resolveTurn :: StateT TetrisGame IO ()  
resolveTurn = do
    currentState <- get
    moveTetromino
    spawnIfNeeded
    return ()

spawnIfNeeded :: StateT TetrisGame IO ()
spawnIfNeeded = do
  currentState <- get
  let mt = tetromino currentState
  case mt of 
    Just t -> return ()
    Nothing -> spawnTetromino  

spawnTetromino :: StateT TetrisGame IO ()
spawnTetromino = do
        currentState <- get
        let (rn, gen') = randomR (0,6) $ tGenerator currentState
        put (currentState { tGenerator = gen', tetromino = Just $ toEnum rn } )
        return ()
        
moveTetromino :: StateT TetrisGame IO ()
moveTetromino = do
        currentState <- get
        let mt = tetromino currentState
        let b = board currentState
        case mt of 
          Nothing -> return ()
          Just t -> case (move currentState) of
            NoMove -> return ()
            MoveLeft -> put $ tryLeftOrRight left currentState t b
            MoveRight -> put $ tryLeftOrRight right currentState t b
            RotateR -> put (currentState { tetromino = Just $ rotateR t, move = NoMove } )
            MoveDown -> put $ tryMovingDown currentState t b

-- Tries to move a tetromino down
-- If a tetromino cannot move down because of a full position on the board the tetromino gets placed on the board
tryMovingDown :: TetrisGame -> Tetromino -> Board -> TetrisGame
tryMovingDown game t b =
  let
    updatedTetromino = down t
  in if (anyFull b (positions updatedTetromino)) 
    then game { board = drawTetrominoToBoard b t, tetromino = Nothing, move = NoMove } 
    else game { tetromino = Just updatedTetromino, move = NoMove }

tryLeftOrRight :: (Tetromino -> Tetromino) -> TetrisGame -> Tetromino -> Board -> TetrisGame
tryLeftOrRight move g t b =
    let
        updatedTetromino = move t
        ps = positions updatedTetromino
    in if isOutOfBounds b ps || anyFull b ps
        then g { move = NoMove } 
        else g { tetromino = Just updatedTetromino, move = NoMove }

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

right :: Tetromino -> Tetromino -- TODO: wall kicks
right tetromino = tetromino { positions = map (\(x,y) -> (x+1,y)) $ positions tetromino }

left :: Tetromino -> Tetromino -- TODO: wall kicks
left tetromino = tetromino { positions = map (\(x,y) -> (x-1,y)) $ positions tetromino }

down :: Tetromino -> Tetromino
down tetromino = 
  let
    postions = positions tetromino
  in 
    tetromino { positions = map (\(x,y) -> (x, y+1)) postions}

-- Output, TODO: Own module

drawGame :: TetrisGame -> String
drawGame game = let
  mt = tetromino game
  b = case mt of 
    Just t -> drawTetrominoToBoard (board game) t
    Nothing -> board game
  lineSize = length $ elems $ b!0
  list2d = chunksOf lineSize $ concat $ map elems $ elems b
  in (unlines $ map lineToString list2d) ++ "\n"

lineToString :: [FieldState] -> String
lineToString fss = intercalate "" $ map (\fs -> if fs == FULL then "X" else " ") fss

drawTetrominoToBoard :: Board -> Tetromino -> Board
drawTetrominoToBoard b t = drawTetrominoToBoardHelper b $ positions t 

drawTetrominoToBoardHelper :: Board -> Positions -> Board
drawTetrominoToBoardHelper b [] = b
drawTetrominoToBoardHelper b ((x,y):xs) = let
  yRow = b ! y
  updatedRow = yRow // [(x, FULL)]
  updatedBoard = b // [(y, updatedRow)]
  in drawTetrominoToBoardHelper updatedBoard xs 
