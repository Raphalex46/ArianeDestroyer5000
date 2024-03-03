module Chess.GameAnalysis where

import Chess.Board
import Chess.Coord
import Chess.Colors
import Chess.Pieces

takeRange :: (a -> Bool) -> [a] -> [a]
takeRange predicate = takeWhile predicate . dropWhile predicate

attackedSquares :: Board -> Coord -> [Coord]
attackedSquares board coord =
  case board ! coord of
    Empty -> []
    Occ p -> attackedSquaresWithPiece board coord p

attackedSquaresWithPiece :: Board -> Coord -> Piece -> [Coord]
attackedSquaresWithPiece board coord@(row, col) (Piece (color, ty)) =
  case ty of
    Pawn ->
      let direction = case color of
                        White -> 1
                        Black -> -1
      in
        -- Check diagonal positions in the direction of the pawn (which is
        -- decided by the color of the pawn)
        filter (isInBounds board) $ map (\x -> (row + direction, col + x)) [-1, 1]
    Rook ->
      let horizontalSquares =
            map fst . takeRange (isNotOppCol . snd) $ getRow board row
          verticalSquares =
            map fst . takeRange (isNotOppCol . snd) $ getCol board col
        in
        horizontalSquares ++ verticalSquares
    Bishop ->
      let slashDiag = map fst . takeRange (isNotOppCol . snd) $ getDiag board coord Slash
          backslashDiag = map fst . takeRange (isNotOppCol . snd) $ getDiag board coord Backslash
      in
      slashDiag ++ backslashDiag
    where
    oppCol = opp color
    isNotOppCol = not . (flip isCol) oppCol
