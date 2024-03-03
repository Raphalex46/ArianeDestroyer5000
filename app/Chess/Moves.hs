module Chess.Moves (Move (..), Side (..)) where

import Chess.Colors
import Chess.Coord
import Chess.Pieces

data Side = QueenSide | KingSide

data Move
  = MovePiece !Coord !Coord
  | Castle !Side
  | Promote !Coord !Coord !PieceType
