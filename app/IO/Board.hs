{- | IO counterpart to the 'Chess.Board' module. It contains functions for
displaying the board in the terminal.
-}
module IO.Board (
  -- | Show functions.
  showBoard,
  showColoredBoard,
  -- | Convert a normal 'Board' to a 'ColoredBoard'.
  toColoredBoard,
  -- | Utilities for 'ColoredBoard'.
  colorSquares,
)
where

import Chess.Board
import Chess.Coord
import Data.List
import qualified System.Console.ANSI as ANSI

-- | A colored square (pairing an ansi color code with a square).
type ColoredSquare = (Maybe ANSI.Color, Square)

-- | An array of colored squares.
type ColoredBoard = Array Coord ColoredSquare

-- | Converts a 'ColoredSquare' to a 'String'.
showColoredSquare :: ColoredSquare -> String
showColoredSquare (Just col, sq) =
  (ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Vivid col])
    ++ (showSquare sq)
    ++ (ANSI.setSGRCode [ANSI.Reset])
showColoredSquare (Nothing, sq) = showSquare sq

-- | Converts a 'ColoredBoard' to a 'String'.
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

-- | Converts a normal 'Board' to a 'String'.
showBoard :: Board -> String
showBoard =
  showColoredBoard . toColoredBoard

-- | Converts a normal 'Board' to a 'ColoredBoard'.
toColoredBoard :: Board -> ColoredBoard
toColoredBoard = fmap (\sq -> (Nothing, sq))

-- | Change the color of the squares at the given coordinates.
colorSquares :: ColoredBoard -> [(Coord, ANSI.Color)] -> ColoredBoard
colorSquares board lst =
  let changes = fmap (\(coord, col) -> (coord, (Just col, snd (board ! coord)))) lst
   in board // changes
