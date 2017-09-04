module Lib where

-- A marking on the game board is either X, O, or empty 
data Mark = X | O | E
  deriving (Show)

-- A row on the game board
data Row = Row Mark Mark Mark 
  deriving (Show)

-- The game board consists of three rows
data Board = Board Row Row Row 

-- String representation for the game board 
instance Show Board where
  show (Board r1 r2 r3) =
    show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3 ++ "\n"

-- Coordinates for rows and columns
data Coord = First | Second | Third

-- Make a play in a row
playRow :: Mark -> Coord -> Row -> Row
playRow move col (Row c1 c2 c3) =
  case col of
    First  -> Row move c2 c3
    Second -> Row c1 move c3
    Third  -> Row c1 c2 move 

-- Make a play on the board
play :: Mark -> (Coord, Coord) -> Board -> Board
play move (row, col) (Board r1 r2 r3) =
  case row of
    First  -> Board (playRow move col r1) r2 r3
    Second -> Board r1 (playRow move col r2) r3
    Third  -> Board r1 r2 (playRow move col r3)

-- New game board
newBoard :: Board
newBoard = Board (Row E E E)
                 (Row E E E)
                 (Row E E E)

-- Examples:
-- play X (First,First) newBoard
-- play X (First,Second) newBoard
-- play X (First,Third) newBoard

-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates

-- What we don't have:
-- Enforce who moves first
-- Turns alternate between X and O players 
-- Protection from repeated plays or overwrites of previous moves
-- More turns not allowed even if a player has already won

