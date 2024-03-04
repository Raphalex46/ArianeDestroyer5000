module Chess.Board
  ( startingBoard,
    Board,
    Square (..),
    showSquare,
    isCol,
    isEmpty,
    isInBounds,
    Dir (..),
    getPartRow,
    getPartCol,
    DiagDir (..),
    getPartDiag,
    movePiece,
    module Data.Array,
  )
where

import Chess.Colors
import Chess.Coord
import Chess.Pieces
import Data.Array
import Data.List
import Data.Maybe

-- | Square datatype
data Square = Empty | Occ !Piece

parseSquare :: Char -> Maybe Square
parseSquare ' ' = Just Empty
parseSquare c = parsePiece c >>= Just . Occ

showSquare :: Square -> String
showSquare Empty = " "
showSquare (Occ p) = showPiece p

-- Some predicates on squares

isCol :: Square -> Color -> Bool
isCol Empty _ = False
isCol (Occ (Piece (pCol, _))) col = col == pCol

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

type Board = Array Coord Square

startingBoard :: Board
startingBoard =
  let list = catMaybes $ map parseSquare ("RNBQKBNR" ++ whitePawns ++ emptyLines ++ blackPawns ++ "rnbqkbnr")
   in array ((0, 0), (7, 7)) $ zip ([(x, y) | x <- [0 .. 7], y <- [0 .. 7]]) list
  where
    whitePawns = "PPPPPPPP"
    blackPawns = "pppppppp"
    emptyLines = replicate (8 * 4) ' '

isInBounds :: Board -> Coord -> Bool
isInBounds board (row, col) =
  lowerRow <= row && row <= upperRow && lowerCol <= col && col <= upperCol
  where
    ((lowerRow, lowerCol), (upperRow, upperCol)) = bounds board

data Dir
  = Pos
  | Neg

getPartRow :: Board -> Dir -> Coord -> [(Coord, Square)]
getPartRow board dir (sRow, sCol) =
  [((sRow, col), board ! (sRow, col)) | col <- [sCol + sign, sCol + sign * 2 .. bound]]
  where
    ((_, lowerCol), (_, upperCol)) = bounds board
    (bound, sign) = case dir of
      Pos -> (upperCol, 1)
      Neg -> (lowerCol, -1)

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
  = NE
  | NW
  | SW
  | SE

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

movePiece :: Board -> Coord -> Coord -> Either String Board
movePiece board src dst
  | not $ (isInBounds board src) && (isInBounds board dst) = Left "Coordinates are out of bounds"
  | otherwise = case board ! src of
      Empty -> Left "The source square is empty"
      sq@(Occ _) -> Right $ board // [(src, Empty), (dst, sq)]
