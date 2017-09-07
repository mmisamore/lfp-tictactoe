{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
module Lib where

import GHC.TypeLits
import Data.Proxy
import Data.Kind
import Data.Either

-- A marking on the game board is either X, O, or empty 
data Mark = X | O | E deriving (Show)

-- Coordinates for rows and columns
data Coord = First | Second | Third

-- The complete board state as a list of plays
data BoardRep = Empty | Cons Mark Coord Coord BoardRep

-- Extreme case: *all* information about board state is at the type level
data Board (rep :: BoardRep) = Board

-- An empty game board
newBoard :: Board 'Empty
newBoard = Board

-- Given a board at the type level, determine who moves next 
type family NextTurn (rep :: BoardRep) :: Mark where
  NextTurn 'Empty           = 'X
  NextTurn ('Cons 'X _ _ _) = 'O
  NextTurn ('Cons 'O _ _ _) = 'X

-- Given a board at the type level, determine if a position has been played
type family Played (row :: Coord) (col :: Coord) (rep :: BoardRep) :: Bool where
  Played _ _ 'Empty                     = 'False
  Played row col ('Cons _ row col prev) = 'True
  Played row col ('Cons _ _ _ prev)     = Played row col prev

-- Given a board at the type level, get the mark at a position
type family BoardVal (row :: Coord) (col :: Coord) (rep :: BoardRep) :: Symbol where
  BoardVal _ _ 'Empty                   = "E"
  BoardVal row col ('Cons 'X row col _) = "X"
  BoardVal row col ('Cons 'O row col _) = "O"
  BoardVal row col ('Cons _ _ _ prev)   = BoardVal row col prev  

-- Convert a board to a type-level string
type family ShowBoard (rep :: BoardRep) :: Symbol where
  ShowBoard rep = 
    AppendSymbol (BoardVal 'First  'First rep ) (
    AppendSymbol (BoardVal 'First  'Second rep) ( 
    AppendSymbol (BoardVal 'First  'Third rep ) (
    AppendSymbol ","                            (
    AppendSymbol (BoardVal 'Second 'First rep ) (
    AppendSymbol (BoardVal 'Second 'Second rep) ( 
    AppendSymbol (BoardVal 'Second 'Third rep ) (
    AppendSymbol ","                            (
    AppendSymbol (BoardVal 'Third  'First rep ) (
    AppendSymbol (BoardVal 'Third  'Second rep) ( 
                 (BoardVal 'Third  'Third rep)))))))))))

-- Helper function: convert a Board to a type-level string
showBoard :: Board (rep :: BoardRep) -> Proxy (ShowBoard rep) 
showBoard Board = Proxy 

-- Convert an empty game board (at type level) to a string 
showEmptyBoard :: String
showEmptyBoard = symbolVal (showBoard newBoard)

-- Lift Coord vals to the type level 
data CoordL (coord :: Coord) where
  First'  :: CoordL 'First
  Second' :: CoordL 'Second
  Third'  :: CoordL 'Third

-- Make a play on the board if player X is next and position is available
xPlay :: (NextTurn rep ~ 'X, Played row col rep ~ 'False) => 
  CoordL row 
  -> CoordL col 
  -> Board (rep :: BoardRep) 
  -> Board ('Cons 'X row col rep) 
xPlay _ _ _ = Board 

-- Make a play on the board if player O is next and position is available
oPlay :: (NextTurn rep ~ 'O, Played row col rep ~ 'False) => 
  CoordL row 
  -> CoordL col 
  -> Board (rep :: BoardRep) 
  -> Board ('Cons 'O row col rep) 
oPlay _ _ _ = Board 

-- Examples:
-- symbolVal (showBoard (xPlay First' First' newBoard))
-- symbolVal (showBoard (oPlay First' First' newBoard))                          -- type error: X always plays first
-- symbolVal (showBoard (xPlay First' First' (xPlay Third' Second' newBoard)))   -- type error: X played twice
-- symbolVal (showBoard (oPlay First' Second' (xPlay First' Second' newBoard)))  -- type error: position already taken
-- symbolVal (showBoard (oPlay Second' Second' (xPlay First' Second' newBoard))) -- successful move 

-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 
-- Protection from repeated plays or overwrites of previous moves

-- What we don't have:
-- More turns not allowed even if a player has already won

