{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Various functions to get information about the board.
module Chess.GameAnalysis (movableSquares, attackedSquares, castleRookPos, isCastlePossible, getSrcCoord, getDstCoord, getRookSide, isKingInCheck, attackedSquaresByColor) where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.Moves
import Chess.Pieces

-- | Small function that acts like 'takeWhile' but adds the first element that doesn't respect the predicate.
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive pre (x : xs) = if pre x then x : takeWhileInclusive pre xs else [x]

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

{- | Get the side on which a rook is.

For simplicity, we consider a rook to be on the queen side if it is on
column 0, and on the king side otherwise.
-}
getRookSide :: Coord -> Side
getRookSide (_, col) = if col == 0 then QueenSide else KingSide

{- | Returns whether castling is possible for a given `Color` and on the given
`Side`.

This function ignores castling rights as it doesn't have access to the
`GameState`, but simply returns `True` if squares between the king and its
destination are empty and are not attacked by ennemy pieces.
-}
isCastlePossible :: Board -> Color -> Side -> Bool
isCastlePossible board color side =
  let (rowSrc, colSrc) = getKingCoord board color
      (_, colDst) = castleKingDst board color side
      dir = signum $ colDst - colSrc
      lowerCoord = min (colSrc + dir) colDst
      upperCoord = max (colSrc + dir) colDst
      attackedSquares = attackedSquaresByColor board (opp color)
   in all (isEmpty . (board !)) [(rowSrc, col) | col <- [lowerCoord .. upperCoord]]
        && all (\coord -> not $ coord `elem` attackedSquares) [(rowSrc, col) | col <- [lowerCoord .. upperCoord]]

-- | Returns whether or not the king of the given `Color` is in check.
isKingInCheck :: Board -> Color -> Bool
isKingInCheck board color =
  let kingPos = getKingCoord board color
   in kingPos `elem` (attackedSquaresByColor board (opp color))

{- | Returns the coordinates of the moving rook when castling the king of a
given `Color` to the given `Side`.
-}
castleRookPos :: Board -> Color -> Side -> Coord
castleRookPos board color side =
  let (kingRow, _) = getKingCoord board color
   in ( kingRow,
        case side of
          QueenSide -> 0
          KingSide -> upperRow board
      )

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

{- | Returns the list of coordinates of squares that are attacked by all pieces
of the given `Color`.
-}
attackedSquaresByColor :: Board -> Color -> [Coord]
attackedSquaresByColor board color =
  concat . map ((attackedSquares board) . fst) $ getSquaresOfCol board color

-- | Returns the list of `Coord` the piece at the given `Coord` can move to.
movableSquares :: Board -> Coord -> [Coord]
movableSquares board coord =
  case board ! coord of
    Empty -> []
    Occ p -> movableSquaresWithPiece board coord p

{- | Returns the list of squares where a given `Piece` can move from the given
`Coord`.
-}
movableSquaresWithPiece :: Board -> Coord -> Piece -> [Coord]
movableSquaresWithPiece board coord@(row, _) piece@(Piece (color, ty)) =
  case ty of
    Pawn ->
      -- Chose direction from the color
      let direction = case color of
            White -> Pos
            Black -> Neg
          -- A pawn at the starting rank can move 2 ranks
          possibleLength =
            if row == pawnStartingRank color
              then 2
              else 1
       in ( map fst . takeWhile (isEmpty . snd) . take possibleLength $
              getPartCol board direction coord
          )
            ++ ( filter
                   ( ((flip isCol) $ opp color)
                       . (board !)
                   )
                   $ attackedSquaresWithPiece board coord piece
               )
    -- For any other piece, the movable squares are the same as the attack
    -- squares, except for squares with allied pieces
    _ -> filter (not . ((flip isCol) color) . (board !)) $ attackedSquaresWithPiece board coord piece

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
