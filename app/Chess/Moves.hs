-- | This module defines a type for the different moves in chess.
module Chess.Moves (Move (..), Side (..), moveIsCapture, moveIsPiece) where

import Chess.Board
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

-- | Checks whether the given `Move` is a capture.
moveIsCapture :: Board -> Move -> Bool
moveIsCapture board (MovePiece src dst) =
  case map (board !) [src, dst] of
    [Occ (Piece (col1, _)), Occ (Piece (col2, _))] -> col1 /= col2
    _ -> False
moveIsCapture _ _ = False

{- | Checks whether the given `Move` is a move involving the given `PieceType`
(this checks only the source piece, not eventual captured pieces).

In case of castling, this returns True for both `Rook` and `King`.
-}
moveIsPiece :: Board -> PieceType -> Move -> Bool
moveIsPiece board pt (MovePiece src _) =
  case board ! src of
    Empty -> False
    Occ (Piece (_, t)) -> pt == t
moveIsPiece _ pt (Castle _ _) = pt == King || pt == Rook
moveIsPiece _ Pawn (Promote _ _ _) = True
moveIsPiece _ _ _ = False
