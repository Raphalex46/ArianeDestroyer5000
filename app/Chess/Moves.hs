-- | This module defines a type for the different moves in chess.
module Chess.Moves (Move (..), Side (..)) where

import Chess.Coord
import Chess.Pieces

-- | Sides for castling.
data Side = QueenSide | KingSide

-- | Move datatype.
data Move
  = -- | Move a piece from one 'Coord' to another (simple move).
    MovePiece !Coord !Coord
  | -- | Castle to the given 'Side'.
    Castle !Side
  | -- | Move a pawn from 'Coord' to another 'Coord' and promote to a new 'PieceType'.
    Promote !Coord !Coord !PieceType
