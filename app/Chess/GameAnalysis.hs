module Chess.GameAnalysis where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.Pieces

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive pre [] = []
takeWhileInclusive pre (x : xs) = if pre x then x : takeWhileInclusive pre xs else [x]

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
       in -- Check diagonal positions in the direction of the pawn (which is
          -- decided by the color of the pawn)
          filter (isInBounds board) $ map (\x -> (row + direction, col + x)) [-1, 1]
    Rook ->
      let horizontalSquares =
            map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartRow board)) coord) $ [Pos, Neg]
          verticalSquares =
            map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartCol board)) coord) $ [Pos, Neg]
       in horizontalSquares ++ verticalSquares
    Bishop ->
      map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartDiag board)) coord) $ [NE, NW, SW, SE]
    Queen ->
      let attack =
            (\x -> attackedSquaresWithPiece board coord (Piece (color, x)))
       in attack Bishop ++ attack Rook
    King ->
      [(tRow, tCol) | (tRow, tCol) <- indices board, (abs $ tRow - row) <= 1, (abs $ tCol - col) <= 1, (tRow /= row) || (tCol /= col)]
    Knight ->
      let offBy1 = filter (\(tRow, tCol) -> ((abs $ tRow - row) == 1) || ((abs $ tCol - col) == 1)) $ indices board
       in filter (\(tRow, tCol) -> ((abs $ tRow - row) == 2) || ((abs $ tCol - col) == 2)) offBy1
  where
    oppCol = opp color
    isNotOppCol = not . (flip isCol) oppCol
