{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
module Lib where

import GHC.TypeLits
import Data.Proxy

-- A marking on the game board is either X, O, or empty 
data Mark = X | O | E deriving (Show)

-- Coordinates for rows and columns
data Coord = First | Second | Third

-- The complete board state as a list of plays
data BoardRep = Empty | Cons Mark Coord Coord BoardRep

-- Game board with both type-level and term-level representation 
-- We choose isomorphic representations here
data Board (rep :: BoardRep) = Board BoardRep

-- An empty game board
newBoard :: Board 'Empty
newBoard = Board Empty

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

-- Lower type-level coordinates to the term level
coordVal :: CoordL (coord :: Coord) -> Coord
coordVal First'  = First
coordVal Second' = Second
coordVal Third'  = Third

-- Given a board at the type level, determine who moves next 
type family NextTurn (rep :: BoardRep) :: Mark where
  NextTurn 'Empty           = 'X
  NextTurn ('Cons 'X _ _ _) = 'O 
  NextTurn ('Cons 'O _ _ _) = 'X 

-- Term-level next turn
nextTurn :: BoardRep -> Mark
nextTurn (Cons O _ _ _) = X
nextTurn (Cons X _ _ _) = O
nextTurn _              = X

-- Given a board at the type level, determine if a position has been played
type family Played (row :: Coord) (col :: Coord) (rep :: BoardRep) :: Bool where
  Played _ _ 'Empty                     = 'False
  Played row col ('Cons _ row col prev) = 'True
  Played row col ('Cons _ _ _ prev)     = Played row col prev

-- Play function that enforces alternating turns 
alternatingPlay :: CoordL row -> CoordL col -> Board rep -> Board ('Cons (NextTurn rep) row col rep)
alternatingPlay row col (Board rep) = Board (Cons (nextTurn rep) (coordVal row) (coordVal col) rep)

-- Examples:
experiment1 = alternatingPlay First' First' newBoard 
experiment2 = alternatingPlay Second' Second' experiment1
experiment3 = symbolVal (showBoard experiment2)

-- Witness for type equality
data Equals a b where
  Refl :: Equals a a

-- Represent proofs for type inequalities 
data Void
data NotEqual a b = NotEqual (Equals a b -> Void)

-- Inductive definition for "board position not played"
data NotPlayed (row :: Coord) (col :: Coord) (rep :: BoardRep) where
  EmptyBoard :: 
    CoordL row  
    -> CoordL col 
    -> Board 'Empty 
    -> NotPlayed row col 'Empty 
  PositionNotPlayed :: 
    CoordL row 
    -> CoordL col 
    -> Board  ('Cons mark r c rep) 
    -> Either (NotEqual row r) (NotEqual col c) 
    -> NotPlayed row col rep 
    -> NotPlayed row col ('Cons mark r c rep) 

-- Discriminate between coordinates at the type-level
type family DiscriminateCoord (c1 :: Coord) (c2 :: Coord) where
  DiscriminateCoord 'First  'First  = () 
  DiscriminateCoord 'Second 'Second = ()
  DiscriminateCoord 'Third  'Third  = ()
  DiscriminateCoord _ _             = Void

-- Construct proofs that coordinates are equal or not
transport :: Equals (c1 :: Coord) (c2 :: Coord) -> 
             DiscriminateCoord c1 c1 ->
             DiscriminateCoord c1 c2
transport Refl d = d

-- Try to prove that two coordinates are not equal at the type level
coordNotEqual :: CoordL (c1 :: Coord) -> CoordL (c2 :: Coord) -> Maybe (NotEqual c1 c2)
coordNotEqual First'  Second'  = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual First'  Third'   = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual Second' First'   = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual Second' Third'   = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual Third'  First'   = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual Third'  Second'  = Just (NotEqual (\eq -> transport eq ()))
coordNotEqual _ _              = Nothing


-- * We need some convenience functions to produce instances of NotPlayed


-- Play function that enforces alternating turns and prohibits positions
-- that were already taken. Proof that position is not already taken is required as input
play ::
  NotPlayed row col rep
  -> CoordL (row :: Coord) 
  -> CoordL (col :: Coord) 
  -> Board  (rep :: BoardRep)
  -> Board  ('Cons (NextTurn rep) row col rep)
play prf row col (Board rep) = Board (Cons (nextTurn rep) (coordVal row) (coordVal col) rep)

-- Examples:
-- experiment4 = play notPlayedPrf4 First' First' newBoard
--   where notPlayedPrf4 = EmptyBoard First' First' newBoard 


-- experiment5 = play (EmptyBoard Second' Second' _ Second' Second' experiment4
-- experiment6 = play Refl First' First' experiment4  -- type error! position not available
-- experiment7 = symbolVal (showBoard experiment5)

-- Some ways to map user inputs to coordinates
maybeFirst :: String -> Maybe (CoordL 'First)
maybeFirst = \case "1" -> Just First'
                   _   -> Nothing
 
maybeSecond :: String -> Maybe (CoordL 'Second)
maybeSecond = \case "2" -> Just Second'
                    _   -> Nothing

maybeThird :: String -> Maybe (CoordL 'Third)
maybeThird = \case "3" -> Just Third'
                   _   -> Nothing


-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 
-- Protection from repeated plays or overwrites of previous moves

-- What we don't have:
-- An implementation that computes new board types at runtime based on user input (dependent types)
