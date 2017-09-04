{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
module Lib where

-- A marking on the game board is either X, O, or empty 
data Mark = X | O | E
  deriving (Show)

-- A row on the game board
data Row = Row Mark Mark Mark 
  deriving (Show)

-- The game board state consists of three rows, and who plays next 
data Board (turn :: Mark) where
  XBoard :: Row -> Row -> Row -> Board 'X 
  OBoard :: Row -> Row -> Row -> Board 'O

-- New empty game board with enforced X as first player
newXBoard :: Board 'X
newXBoard = XBoard (Row E E E) (Row E E E) (Row E E E)

-- New empty game board with enforced X as first player
newOBoard :: Board 'O
newOBoard = OBoard (Row E E E) (Row E E E) (Row E E E) 

-- Helper to show game board rows 
showRow :: Row -> Row -> Row -> String
showRow r1 r2 r3 = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3 ++ "\n"

-- String representation for the game board 
instance Show (Board t) where
  show (XBoard r1 r2 r3) = showRow r1 r2 r3 
  show (OBoard r1 r2 r3) = showRow r1 r2 r3 
 
-- Coordinates for rows and columns
data Coord = First | Second | Third

-- Make a play in a row
playRow :: Mark -> Coord -> Row -> Row
playRow move col (Row c1 c2 c3) =
  case col of
    First  -> Row move c2 c3
    Second -> Row c1 move c3
    Third  -> Row c1 c2 move 

-- A play by the X player results in board where O gets next play
xPlay :: (Coord, Coord) -> Board 'X -> Board 'O
xPlay (row, col) (XBoard r1 r2 r3) =
  case row of
    First  -> OBoard (playRow X col r1) r2 r3
    Second -> OBoard r1 (playRow X col r2) r3
    Third  -> OBoard r1 r2 (playRow X col r3)

-- A play by the O player results in board where X gets next play
oPlay :: (Coord, Coord) -> Board 'O -> Board 'X
oPlay (row, col) (OBoard r1 r2 r3) =
  case row of 
    First  -> XBoard (playRow O col r1) r2 r3
    Second -> XBoard r1 (playRow O col r2) r3
    Third  -> XBoard r1 r2 (playRow O col r3)

-- Examples:
-- xPlay (First,First) newXBoard
-- oPlay (First,First) newXBoard  -- type error! 
-- oPlay (Second,Second) (xPlay (First,First) newXBoard)
-- oPlay (Third,Third) $ oPlay (Second,Second) $ xPlay (First,First) newXBoard

-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 

-- What we don't have:
-- Protection from repeated plays or overwrites of previous moves
-- More turns not allowed even if a player has already won

