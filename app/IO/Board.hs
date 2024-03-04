module IO.Board (showBoard, showColoredBoard, toColoredBoard, colorSquares) where

import Chess.Board
import Chess.Coord
import Data.Array
import Data.List
import qualified System.Console.ANSI as ANSI

type ColoredSquare = (Maybe ANSI.Color, Square)

type ColoredBoard = Array Coord ColoredSquare

showColoredSquare :: ColoredSquare -> String
showColoredSquare (Just col, sq) =
  (ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Vivid col])
    ++ (showSquare sq)
    ++ (ANSI.setSGRCode [ANSI.Reset])
showColoredSquare (Nothing, sq) = showSquare sq

showColoredBoard :: ColoredBoard -> String
showColoredBoard board =
  unlines $ [columnNames, separator] ++ (intersperse (separator) $ map showRow [7, 6 .. 0]) ++ [separator, columnNames]
  where
    hspace = " "
    columnNames = hspace ++ (concat $ map (\x -> "  " ++ [x] ++ " ") ['A' .. 'H'])
    separator = hspace ++ (take 32 $ cycle "+---") ++ "+"
    showRow row =
      let rowSquares = [board ! (row, i) | i <- [0 .. 7]]
       in (show $ row + 1) ++ (concat $ map (\x -> "| " ++ showColoredSquare x ++ " ") rowSquares) ++ "|" ++ (show $ row + 1)

showBoard :: Board -> String
showBoard =
  showColoredBoard . toColoredBoard

toColoredBoard :: Board -> ColoredBoard
toColoredBoard = fmap (\sq -> (Nothing, sq))

colorSquares :: ColoredBoard -> [(Coord, ANSI.Color)] -> ColoredBoard
colorSquares board lst =
  let changes = fmap (\(coord, col) -> (coord, (Just col, snd (board ! coord)))) lst
   in board // changes
