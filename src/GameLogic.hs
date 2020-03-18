module GameLogic
( runTurn
, addTetrominoToBoard
, moveDownVector
) where
  
import Control.Monad.State
import System.Random
import Data.Array
import Data.List
import Data.Ord
import Tetromino
import Game
import qualified Data.Vector as V

runTurn = execStateT resolveTurn

resolveTurn :: StateT TetrisGame IO ()  
resolveTurn = do
    moveTetromino
    spawnIfNeeded
    updateScore
    handleFullRows
    return ()
    
spawnIfNeeded :: StateT TetrisGame IO ()
spawnIfNeeded = do
  currentState <- get
  let mt = tetromino currentState
  case mt of 
    Just t -> return ()
    Nothing -> spawnTetromino  

updateScore :: StateT TetrisGame IO ()
updateScore = do
  currentState <- get
  let currentScoring = scoring currentState
  put (currentState { scoring = (<>) currentScoring (calculateScore currentState) })  

handleFullRows :: StateT TetrisGame IO ()
handleFullRows = do
  currentState <- get
  let width = boardWidth $ board currentState
  let moveDownV = moveDownVector (V.map (>=width) (horizontalCount currentState))
  let updatedBoard = removeFullRows (board currentState) moveDownV 
  let updatedHorizontalCount = calculateHorizontalCount updatedBoard
  put (currentState { board = updatedBoard, horizontalCount = updatedHorizontalCount })  

-- TODO: Check if Tetromino spawns on another tetromino
-- If this is the case -> game over
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
            MoveLeft -> put $ tryMove left currentState t b
            MoveRight -> put $ tryMove right currentState t b
            RotateR -> put $ tryMove rotateR currentState t b
            MoveDown -> put $ tryMovingDown currentState t b

-- Tries to move a tetromino down
-- If a tetromino cannot move down because of a full position on the board the tetromino gets placed on the board
tryMovingDown :: TetrisGame -> Tetromino -> Board -> TetrisGame
tryMovingDown game t b =
  let
    updatedTetromino = down t
    ps = positions updatedTetromino
    currentHorizontalCount = horizontalCount game   
  in if (anyBeyondBottom b ps || anyFull b ps) 
    then 
      if (any (==0) (map snd (positions t))) -- Cannot move down and y position is still zero -> game over
        then error "Game Over" 
        else game { board = addTetrominoToBoard b t, tetromino = Nothing, move = NoMove, horizontalCount = updateHorizontalCountVector currentHorizontalCount (positions t) } 
    else game { tetromino = Just updatedTetromino, move = NoMove }

tryMove :: (Tetromino -> Tetromino) -> TetrisGame -> Tetromino -> Board -> TetrisGame
tryMove move g t b =
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

addTetrominoToBoard :: Board -> Tetromino -> Board
addTetrominoToBoard b t = addTetrominoToBoardHelper b $ positions t 

addTetrominoToBoardHelper :: Board -> Positions -> Board
addTetrominoToBoardHelper b [] = b
addTetrominoToBoardHelper b ((x,y):xs) 
  | x<0 || y<0 = addTetrominoToBoardHelper b xs
  | otherwise = addTetrominoToBoardHelper updatedBoard xs
  where 
    yRow = b ! y
    updatedRow = yRow // [(x, FULL)]
    updatedBoard = b // [(y, updatedRow)]

clearRows :: Board -> [Int] -> Board  
clearRows b indices = 
  let
    widthOuterBound = snd $ bounds $ (b ! 0)
    emptyRow = listArray (0, widthOuterBound) $ take (10) $ repeat EMPTY
  in b // (map (\i-> (i, emptyRow))) indices

-- TODO: Refactoring and proper documentation  
-- V.Vector Int - vector containing the move down count for each element in board
-- TODO: Fill first n rows with empty rows where n is number of full rows in b 
removeFullRows :: Board -> V.Vector Int -> Board  
removeFullRows b moveDownVector = 
  let
    indexedMoveDownList= V.toList $ V.indexed moveDownVector
    height = boardHeight b
    updateList = map (\(i, m) -> ((height-i)+m, b ! (height-i))) indexedMoveDownList -- move row at index i down m times
  in b // updateList

moveDownVector ::  V.Vector Bool -> V.Vector Int
moveDownVector hfc = moveDownVectorHelper (V.reverse hfc) (V.fromList $ take (V.length hfc) $ repeat 0) 0 0

type Index = Int
type CurrentMoveDownVector = V.Vector Int
type CurrentMoveDownCount = Int

moveDownVectorHelper :: V.Vector Bool -> CurrentMoveDownVector -> Index -> CurrentMoveDownCount -> V.Vector Int
moveDownVectorHelper hfc cmdv i cmdc
  | i >= V.length hfc = cmdv
  | otherwise = if (hfc V.! i == True) 
    then moveDownVectorHelper hfc (cmdv V.// [(i, cmdc)]) (i+1) (cmdc+1)
    else moveDownVectorHelper hfc (cmdv V.// [(i, cmdc)]) (i+1) cmdc