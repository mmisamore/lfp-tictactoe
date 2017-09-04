{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
module Lib where

-- A marking on the game board is either X, O, or empty 
data Mark = X | O | E
  deriving (Show)

-- A row on the game board
data Row = Row Mark Mark Mark 
  deriving (Show)

-- Coordinates for rows and columns
data CoordType = First | Second | Third

-- A type-level representation of previous moves
data PreviousMoves = Empty | Cons Mark CoordType CoordType PreviousMoves

-- The game board state consists of the board and all previous moves
data Board (prev :: PreviousMoves) = Board Row Row Row

-- New empty game board with no previous moves
newBoard :: Board Empty
newBoard = Board (Row E E E) (Row E E E) (Row E E E)

-- String representation for the game board 
instance Show (Board prev) where
  show (Board r1 r2 r3) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3 ++ "\n"
 
-- Make a play in a row
playRow :: Mark -> CoordType -> Row -> Row
playRow move col (Row c1 c2 c3) =
  case col of
    First  -> Row move c2 c3
    Second -> Row c1 move c3
    Third  -> Row c1 c2 move 

-- Given a sequence of previous moves, determine whose turn is next
-- This is a type-level function PreviousMoves -> Mark
type family NextTurn (prev :: PreviousMoves) :: Mark where
  NextTurn (Cons 'X _ _ _) = 'O
  NextTurn (Cons 'O _ _ _) = 'X
  NextTurn Empty         = 'X  -- assume X plays first 

-- Given a sequence of previous moves, determine if a position has been played already
type family Played (row :: CoordType) (col :: CoordType) (prev :: PreviousMoves) :: Bool where
  Played _ _ 'Empty                  = 'False
  Played row col ('Cons _ row col _) = 'True 
  Played row col ('Cons _ _ _ prev)  = Played row col prev

-- Lift our Coord type up to a Coord "kind". Allows us to use coordinates at the type
-- level, which is required to describe previous moves  
data CoordKind (a :: CoordType) where
   First'  :: CoordKind 'First
   Second' :: CoordKind 'Second
   Third'  :: CoordKind 'Third 

-- Lower a *type* in the Coord kind back down to a *term* in the Coord type
-- Note: in a full dependently typed language like Idris, we don't have to care about 
-- the difference between terms, types, and kinds anymore 
coordVal :: CoordKind a -> CoordType
coordVal First'  = First
coordVal Second' = Second  
coordVal Third'  = Third

-- If the position was not already played, and player X has next turn, allow a play
-- that updates the board with an X 
xPlay :: (Played row col prev ~ 'False, NextTurn prev ~ 'X) => 
  (CoordKind row, CoordKind col) -> Board prev -> Board ('Cons 'X row col prev) 
xPlay (row, col) (Board r1 r2 r3) =
  let (valRow, valCol) = (coordVal row, coordVal col) 
  in case valRow of   
     First  -> Board (playRow X valCol r1) r2 r3
     Second -> Board r1 (playRow X valCol r2) r3
     Third  -> Board r1 r2 (playRow X valCol r3)

-- If the position was not already played, and player O has next turn, allow a play
-- that updates the board with an O 
oPlay :: (Played row col prev ~ 'False, NextTurn prev ~ 'O) => 
  (CoordKind row, CoordKind col) -> Board prev -> Board ('Cons 'O row col prev) 
oPlay (row, col) (Board r1 r2 r3) =
  let (valRow, valCol) = (coordVal row, coordVal col) 
  in case valRow of   
     First  -> Board (playRow O valCol r1) r2 r3
     Second -> Board r1 (playRow O valCol r2) r3
     Third  -> Board r1 r2 (playRow O valCol r3)

-- Examples:
-- xPlay (First',First') newBoard
-- oPlay (First',First') newBoard
-- xPlay (First',First') (xPlay (Third',Second') newBoard) -- type error!
-- oPlay (First',First') $ xPlay (Third',Second') newBoard
-- oPlay (Third',Second') $ xPlay (Third',Second') newBoard

-- What we have so far:
-- Each board location must be one of: X, O, or E
-- Safe indexing into the board via coordinates
-- Enforce who moves first
-- Turns alternate between X and O players 
-- Protection from repeated plays or overwrites of previous moves

-- What we don't have:
-- More turns not allowed even if a player has already won

