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

-- Lower CoordL down to term level
coordVal :: CoordL (coord :: Coord) -> Coord
coordVal First'  = First
coordVal Second' = Second 
coordVal Third'  = Third

-- Witness for type equality
data Equals a b where
  Refl :: Equals a a

-- Type equality is reflexive
refl :: Equals a a
refl = Refl

-- Type equality is symmetric
sym :: Equals a b -> Equals b a
sym Refl = Refl

-- Type equality is transitive
trans :: Equals b c -> Equals a b -> Equals a c
trans Refl Refl = Refl

-- Flipping marks
type family Flip (a :: Mark) :: Mark where
  Flip 'X = 'O
  Flip 'O = 'X

-- Given a board at the type level, determine who moves next 
type family NextTurn (rep :: BoardRep) :: Mark where
  NextTurn 'Empty          = 'X
  NextTurn ('Cons a _ _ _) = Flip a 

-- Given a board at the type level, determine if a position has been played
type family Played (row :: Coord) (col :: Coord) (rep :: BoardRep) :: Bool where
  Played _ _ 'Empty                     = 'False
  Played row col ('Cons _ row col prev) = 'True
  Played row col ('Cons _ _ _ prev)     = Played row col prev

-- Reify constraints as equality types, chain along the next constraint 
play :: 
  Equals (NextTurn rep) a
  -> CoordL row 
  -> CoordL col 
  -> Board (rep :: BoardRep) 
  -> (Board ('Cons a row col rep), Equals (NextTurn ('Cons a row col rep)) (Flip a)) 
play Refl row col Board = (Board, Refl)

-- Another constraint: Played row col rep ~ 'False 

-- Play Examples:
experiment1 = play Refl First' First' newBoard
experiment2 = let (board1, eq1) = experiment1
              in play eq1 Second' Second' board1
experiment3 = let (board2, eq2) = experiment2
              in play eq2 Third' Third' board2

-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 

-- What we don't have:
-- Protection from repeated plays or overwrites of previous moves
-- An implementation that computes required types at runtime based on user input (dependent types)
