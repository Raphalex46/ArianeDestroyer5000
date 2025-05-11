-- | This module defines a type for the different moves in chess.
module Chess.Moves (Move (..), Side (..), moveIsCapture, moveIsPiece, castleKingDst, fromAlgebraic, toAlgebraic, getSrcCoord, getDstCoord) where

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

{- | Returns the destination of the king for a castle of the given `Color` to
the given `Side`.

The king always moves two columns when castling.
-}
castleKingDst :: Board -> Color -> Side -> Coord
castleKingDst board color side =
  let (row, col) = getKingCoord board color
   in case side of
        KingSide -> (row, col + 2)
        QueenSide -> (row, col - 2)

fromAlgebraic :: Board -> String -> Move
fromAlgebraic board str =
  case str of
    [sc, sr, dc, dr] ->
      let (source, dest) = parseSrcDst [sc, sr, dc, dr]
       in case (board ! source) of
            Occ (Piece (col, King)) ->
              let side = filter ((dest ==) . fst) $ map (\s -> (castleKingDst board col s, s)) [QueenSide, KingSide]
               in case side of
                    [] -> MovePiece source dest
                    (_, s) : _ -> Castle col s
            _ -> MovePiece source dest
    [sc, sr, dc, dr, p] ->
      let (source, dest) = parseSrcDst [sc, sr, dc, dr]
          pt = case parsePieceType p of
            Nothing -> error parseError
            Just t -> t
       in Promote source dest pt
    _ -> error parseError
 where
  parseError = "invalid algebraic move"
  parseSrcDst coords =
    case coords of
      [sc, sr, dc, dr] ->
        let source = case parseCoord [sc, sr] of
              Nothing -> error parseError
              Just c -> c
            dest = case parseCoord [dc, dr] of
              Nothing -> error parseError
              Just c -> c
         in (source, dest)
      _ -> error parseError

toAlgebraic :: Board -> Move -> String
toAlgebraic board move =
  case move of
    Promote src dst pt -> showCoord src ++ showCoord dst ++ (showPiece $ Piece (Black, pt))
    m -> showCoord (getSrcCoord board m) ++ showCoord (getDstCoord board m)

{- | Get source coordinates of the given `Move`.

For any 'normal' move, returns the coordinate of the source moved piece.
For a castle, returns the coordinate of the castling king.
-}
getSrcCoord :: Board -> Move -> Coord
getSrcCoord _ (MovePiece src _) = src
getSrcCoord _ (Promote src _ _) = src
getSrcCoord board (Castle color _) = getKingCoord board color

-- | Get destination coordinates of the given `Move`.
getDstCoord :: Board -> Move -> Coord
getDstCoord _ (MovePiece _ dst) = dst
getDstCoord _ (Promote _ dst _) = dst
getDstCoord board (Castle color side) = castleKingDst board color side
