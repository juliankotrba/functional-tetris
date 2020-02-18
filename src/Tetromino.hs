module Tetromino
( TetrominoType (..)
, Tetromino (..)
, Position
, Positions
, defaultI
, defaultO
, defaultT
, defaultJ
, defaultL
, defaultS
, defaultZ
) where 

type Position = (Int, Int)
type Positions = [Position]

data TetrominoType = I | O | T | J | L | S | Z deriving (Show, Eq, Ord)

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

  fromEnum (Tetromino I _) = 0
  fromEnum (Tetromino O _) = 1
  fromEnum (Tetromino T _) = 2
  fromEnum (Tetromino J _) = 3
  fromEnum (Tetromino L _) = 4
  fromEnum (Tetromino S _) = 5
  fromEnum (Tetromino Z _) = 6

-- Default Tetrominos
-- The rotation point of each tetromino is placed at index 0
defaultI = Tetromino I [(1,0), (0,0), (2,0), (3,0)]
defaultO = Tetromino O [(0,0), (1,0), (0,1), (1,1)]
defaultT = Tetromino T [(1,1), (1,0), (0,1), (2,1)]
defaultJ = Tetromino J [(1,1), (2,0), (0,1), (2,1)]
defaultL = Tetromino L [(1,1), (0,0), (0,1), (2,1)]
defaultS = Tetromino S [(1,1), (1,0), (2,0), (0,1)]
defaultZ = Tetromino Z [(1,1), (0,0), (1,0), (2,1)]