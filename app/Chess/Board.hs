module Chess.Board (startingBoard, showBoard) where

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

movePiece :: Board -> Coord -> Coord -> Either String Board
movePiece board src dst
  | not $ (isInBounds board src) && (isInBounds board dst) = Left "Coordinates are out of bounds"
  | otherwise = case board ! src of
      Empty -> Left "The source square is empty"
      sq@(Occ _) -> Right $ board // [(src, Empty), (dst, sq)]
