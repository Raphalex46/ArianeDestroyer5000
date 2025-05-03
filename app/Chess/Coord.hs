-- | Chess coordinates type definition and associated functions.
module Chess.Coord (Coord, parseCoord) where

import Data.Char
import Data.List
import Text.Read

-- | Basic type alias for coordinates.
type Coord = (Int, Int)

{- | Converts a column number to a letter.

Returns 'Nothing' if the given column is out of bounds of a chess board.
-}
_colToLet :: Int -> Maybe Char
_colToLet i
  | 0 <= i && i <= 7 = find (\x -> ord x == i - ord 'A') ['A' .. 'H']
  | otherwise = Nothing

{- | Converts a letter to a column number.

Returns 'Nothing' if the given letter is not a valid column in chess.
-}
letToCol :: Char -> Maybe Int
letToCol c
  | toUpper c `elem` ['A' .. 'H'] = Just $ ord (toUpper c) - ord 'A'
  | otherwise = Nothing

-- | Converts a string to a coordinate.
parseCoord :: String -> Maybe Coord
parseCoord [c, r] =
  do
    col <- letToCol c
    row <- readMaybe [r]
    -- Similar error handling for columns is taken care of in 'letToCol'.
    if 1 <= row && row <= 8
      then Just (row - 1, col)
      else Nothing
parseCoord _ = Nothing
