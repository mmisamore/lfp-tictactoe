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

-- Type-level game board 
data Board (rep :: BoardRep) = Board

-- An empty game board
newBoard :: Board 'Empty
newBoard = Board 

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
showBoard _ = Proxy 

-- Convert an empty game board (at type level) to a string 
showEmptyBoard :: String
showEmptyBoard = symbolVal (showBoard newBoard)

-- Lift Coord vals to the type level 
data CoordL (coord :: Coord) where
  First'  :: CoordL 'First
  Second' :: CoordL 'Second
  Third'  :: CoordL 'Third

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

-- Play function that enforces alternating turns 
alternatingPlay :: 
  CoordL row 
  -> CoordL col 
  -> Board (rep :: BoardRep) 
  -> Board ('Cons (NextTurn rep) row col rep)
alternatingPlay row col Board = Board

-- Examples:
experiment1 = alternatingPlay First' First' newBoard 
experiment2 = alternatingPlay Second' Second' experiment1
experiment3 = symbolVal (showBoard experiment2)

-- Witness for type equality
data Equals a b where
  Refl :: Equals a a

-- Play function that enforces alternating turns and prohibits positions
-- that were already taken. Proof that position is not already taken is required as input
play ::
  Equals (Played row col rep) 'False
  -> CoordL row
  -> CoordL col
  -> Board (rep :: BoardRep)
  -> Board ('Cons (NextTurn rep) row col rep)
play Refl row col Board = Board

-- Examples:
experiment4 = play Refl First' First' newBoard
experiment5 = play Refl Second' Second' experiment4
-- experiment6 = play Refl First' First' experiment4  -- type error! position not available
experiment7 = symbolVal (showBoard experiment5)


-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 
-- Protection from repeated plays or overwrites of previous moves

-- What we don't have:
-- An implementation that computes new board types at runtime based on user input (dependent types)

