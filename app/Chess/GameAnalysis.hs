-- | Various functions to get information about the board.
module Chess.GameAnalysis where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.Pieces

-- | Small funciton that acts like 'takeWhile' but adds the first element that doesn't respect the predicate.
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive pre (x : xs) = if pre x then x : takeWhileInclusive pre xs else [x]

-- | This function calls 'attackedSquaresWithPiece' with the piece at the given 'Coord'.
attackedSquares :: Board -> Coord -> [Coord]
attackedSquares board coord =
  case board ! coord of
    Empty -> []
    Occ p -> attackedSquaresWithPiece board coord p

-- | Returns the list of 'Coord' that would be attacked by the given 'Piece' at the given 'Coord'.
attackedSquaresWithPiece :: Board -> Coord -> Piece -> [Coord]
attackedSquaresWithPiece board coord@(row, col) (Piece (color, ty)) =
  case ty of
    -- For pawns, we need to check the color to know their direction
    Pawn ->
      let direction = case color of
            White -> 1
            Black -> -1
       in -- Check diagonal positions in the direction of the pawn (which is
          -- decided by the color of the pawn)
          filter (isInBounds board) $ map (\x -> (row + direction, col + x)) [-1, 1]
    -- Check every row and file starting from the rook and stop at the first piece
    Rook ->
      let horizontalSquares =
            map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartRow board)) coord) $ [Pos, Neg]
          verticalSquares =
            map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartCol board)) coord) $ [Pos, Neg]
       in horizontalSquares ++ verticalSquares
    -- Same as the rook but for diagonals
    Bishop ->
      map fst . concat . map (takeWhileInclusive (isEmpty . snd) . (flip (getPartDiag board)) coord) $ [NE, NW, SW, SE]
    -- Easiest piece: just a combination of rook and bishop conditions
    Queen ->
      let attack =
            (\x -> attackedSquaresWithPiece board coord (Piece (color, x)))
       in attack Bishop ++ attack Rook
    -- The king simply attacks every square around
    King ->
      [(tRow, tCol) | (tRow, tCol) <- indices board, (abs $ tRow - row) <= 1, (abs $ tCol - col) <= 1, (tRow /= row) || (tCol /= col)]
    -- Knight is a bit weird, but this works
    Knight ->
      let offBy1 = filter (\(tRow, tCol) -> ((abs $ tRow - row) == 1) || ((abs $ tCol - col) == 1)) $ indices board
       in filter (\(tRow, tCol) -> ((abs $ tRow - row) == 2) || ((abs $ tCol - col) == 2)) offBy1
