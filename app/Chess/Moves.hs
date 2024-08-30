-- | This module defines a type for the different moves in chess.
module Chess.Moves (Move (..), Side (..)) where

import Chess.Colors
import Chess.Coord
import Chess.Pieces

-- | Sides for castling.
data Side = QueenSide | KingSide deriving (Eq, Show)

-- | Move datatype.
data Move
  = -- | Move a piece from one 'Coord' to another (simple move).
    MovePiece !Coord !Coord
  | -- | Castle the king of the given `Color` to the given 'Side'.
    Castle !Color !Side
  | -- | Move a pawn from 'Coord' to another 'Coord' and promote to a new 'PieceType'.
    Promote !Coord !Coord !PieceType
  deriving (Eq, Show)
