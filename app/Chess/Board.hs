module Chess.Board
  ( startingBoard,
    showBoard,
    Board,
    Square (..),
    isCol,
    isInBounds,
    getRow,
    getCol,
    DiagType (..),
    getDiag,
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

type Board = Array Coord Square

startingBoard :: Board
startingBoard =
  let list = catMaybes $ map parseSquare ("RNBQKBNR" ++ whitePawns ++ emptyLines ++ blackPawns ++ "rnbqkbnr")
   in array ((0, 0), (7, 7)) $ zip ([(x, y) | x <- [0 .. 7], y <- [0 .. 7]]) list
  where
    whitePawns = "PPPPPPPP"
    blackPawns = "pppppppp"
    emptyLines = replicate (8 * 4) ' '

showBoard :: Board -> String
showBoard board =
  unlines $ [columnNames, separator] ++ (intersperse (separator) $ map showRow [7, 6 .. 0]) ++ [separator, columnNames]
  where
    hspace = " "
    columnNames = hspace ++ (concat $ map (\x -> "  " ++ [x] ++ " ") ['A' .. 'H'])
    separator = hspace ++ (take 32 $ cycle "+---") ++ "+"
    showRow row =
      let rowSquares = [board ! (row, i) | i <- [0 .. 7]]
       in (show $ row + 1) ++ (concat $ map (\x -> "| " ++ showSquare x ++ " ") rowSquares) ++ "|" ++ (show $ row + 1)

isInBounds :: Board -> Coord -> Bool
isInBounds board (row, col) =
  lowerRow <= row && row <= upperRow && lowerCol <= col && col <= upperCol
  where
    ((lowerRow, lowerCol), (upperRow, upperCol)) = bounds board

getRow :: Board -> Int -> [(Coord, Square)]
getRow board row = filter ((== row) . fst . fst) $ assocs board

getCol :: Board -> Int -> [(Coord, Square)]
getCol board col = filter ((== col) . snd . fst) $ assocs board

-- | Enum for diagonal directions.
data DiagType
  = -- | For diagonals like '/'
    Slash
  | -- | For diagonals like '\'
    Backslash
  | -- | For both types of diagonals
    Both

getDiag :: Board -> Coord -> DiagType -> [(Coord, Square)]
getDiag board coord@(row, col) diagTy =
  case diagTy of
    Both -> filter (isDistEqual coord . fst) $ assocs board
    Slash -> filter ((\target -> isSignEqual coord target && isDistEqual coord target) . fst) $ assocs board
    Backslash -> filter (not . (\target -> isSignEqual coord target && isDistEqual coord target) . fst) $ assocs board
  where
    isDistEqual (sRow, sCol) (tRow, tCol) =
      abs (tCol - sCol) == abs (tRow - sRow)
    isSignEqual (sRow, sCol) (tRow, tCol) =
      signum (tCol - sCol) == signum (tRow - sRow)

movePiece :: Board -> Coord -> Coord -> Either String Board
movePiece board src dst
  | not $ (isInBounds board src) && (isInBounds board dst) = Left "Coordinates are out of bounds"
  | otherwise = case board ! src of
      Empty -> Left "The source square is empty"
      sq@(Occ _) -> Right $ board // [(src, Empty), (dst, sq)]
