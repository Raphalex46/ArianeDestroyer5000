-- | This module contains the definitions for squares and the board, with
-- basic operations on the board and predicates on squares.
module Chess.Board
  ( -- | Types.
    Square (..),
    Board,
    -- | Utilities for the board.
    isInBounds,
    movePiece,
    removePiece,
    -- | Starting board.
    startingBoard,
    showSquare,
    -- | Predicates on squares.
    isCol,
    isEmpty,
    isPieceType,
    -- | Fetching rows and diagonals.
    Dir (..),
    getPartRow,
    getPartCol,
    DiagDir (..),
    getPartDiag,
    -- | Reexported module for more advanced board manipulations.
    module Data.Array,
  )
where

import Chess.Colors
import Chess.Coord
import Chess.Pieces
import Data.Array
import Data.Maybe

-- | Square datatype.
data Square
  = -- | Empty square.
    Empty
  | -- | Square occupied with a piece.
    Occ !Piece

-- | Parse a square from a character.
--
-- * A space corresponds to an empty square.
-- * Another character is interpreted like 'Chess.Pieces.parsePiece'.
parseSquare :: Char -> Maybe Square
parseSquare ' ' = Just Empty
parseSquare c = parsePiece c >>= Just . Occ

-- | Converts a square to string representation.
showSquare :: Square -> String
showSquare Empty = " "
showSquare (Occ p) = showPiece p

-- | Tests if the square contains a piece of a given color.
isCol :: Square -> Color -> Bool
isCol Empty _ = False
isCol (Occ (Piece (pCol, _))) col = col == pCol

-- | Tests if the square is empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Tests if the `square` has a piece of the given `PieceType`.
isPieceType :: Square -> PieceType -> Bool
isPieceType Empty _ = False
isPieceType (Occ (Piece (_, pTy))) ty = ty == pTy

-- | Type alias for a board. A board is simply an array of squares.
type Board = Array Coord Square

-- | Returns a board in the starting position of a normal chess game.
startingBoard :: Board
startingBoard =
  -- Generate a standard position my using the 'parseSquare' function.
  let list = catMaybes $ map parseSquare ("RNBQKBNR" ++ whitePawns ++ emptyLines ++ blackPawns ++ "rnbqkbnr")
   in array ((0, 0), (7, 7)) $ zip ([(x, y) | x <- [0 .. 7], y <- [0 .. 7]]) list
  where
    whitePawns = "PPPPPPPP"
    blackPawns = "pppppppp"
    emptyLines = replicate (8 * 4) ' '

-- | Tests whether or not the given 'Coord' is in the bounds of the given 'Board'.
isInBounds :: Board -> Coord -> Bool
isInBounds board (row, col) =
  -- Basically just check every bound.
  lowerRow <= row && row <= upperRow && lowerCol <= col && col <= upperCol
  where
    ((lowerRow, lowerCol), (upperRow, upperCol)) = bounds board

-- | Direction when fetching a row or column (file).
data Dir
  = -- | Positive direction (to the right or up).
    Pos
  | -- | Negative direction (to the left or bottom).
    Neg

-- | Return a partial row of the board, starting from the given 'Coord' and
-- going in the given 'Dir' until getting to the edge of the board.
getPartRow :: Board -> Dir -> Coord -> [(Coord, Square)]
getPartRow board dir (sRow, sCol) =
  [((sRow, col), board ! (sRow, col)) | col <- [sCol + sign, sCol + sign * 2 .. bound]]
  where
    ((_, lowerCol), (_, upperCol)) = bounds board
    (bound, sign) = case dir of
      Pos -> (upperCol, 1)
      Neg -> (lowerCol, -1)

-- | Return a partial column (file) of the board, starting from the given
-- 'Coord' and going in the given 'Dir' until getting to the edge of the board.
getPartCol :: Board -> Dir -> Coord -> [(Coord, Square)]
getPartCol board dir (sRow, sCol) =
  [((row, sCol), board ! (row, sCol)) | row <- [sRow + sign, sRow + sign * 2 .. bound]]
  where
    ((lowerRow, _), (upperRow, _)) = bounds board
    (bound, sign) = case dir of
      Pos -> (upperRow, 1)
      Neg -> (lowerRow, -1)

-- | Enum for diagonal directions.
data DiagDir
  = -- | Northeast.
    NE
  | -- | Northwest.
    NW
  | -- | Southwest.
    SW
  | -- | Southeast.
    SE

-- | Get a partial diagonal. Similar to 'getPartRow' and 'getPartCol'.
getPartDiag :: Board -> DiagDir -> Coord -> [(Coord, Square)]
getPartDiag board dir (sRow, sCol) =
  map (\x -> (x, board ! x)) $
    takeWhile (isInBounds board) [(sRow + y, sCol + x) | (y, x) <- zip [signY, signY * 2 ..] [signX, signX * 2 ..]]
  where
    (signY, signX) = case dir of
      NE -> (1, 1)
      NW -> (1, -1)
      SW -> (-1, -1)
      SE -> (-1, 1)

-- | Move a piece from one position to another. This doesn't check the rules.
-- TODO: Remove the error checking and put a pre-condition. This code
-- shouldn't be called with invalid coordinate values
movePiece :: Board -> Coord -> Coord -> Either String Board
movePiece board src dst
  | not $ (isInBounds board src) && (isInBounds board dst) = Left "Coordinates are out of bounds"
  | otherwise = case board ! src of
      Empty -> Left "The source square is empty"
      sq@(Occ _) -> Right $ board // [(src, Empty), (dst, sq)]

removePiece :: Board -> Coord -> Board
removePiece board coord = board // [(coord, Empty)]
