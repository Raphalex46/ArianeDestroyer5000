-- | Chess coordinates type definition and associated functions.
module Chess.Coord (Coord, parseCoord, distanceSq, showCoord) where

import Data.Char
import Data.List
import Text.Read

-- | Basic type alias for coordinates.
type Coord = (Int, Int)

{- | Converts a column number to a letter.

Returns 'Nothing' if the given column is out of bounds of a chess board.
-}
colToLet :: Int -> Maybe Char
colToLet i
  | (0 <= i) && (i <= 7) = find (\x -> ord x == ord 'a' + i) ['a' .. 'h']
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

-- | Print a coordinate (column letter + row number).
showCoord :: Coord -> String
showCoord (x, y) =
  case colToLet y of
    Nothing -> error "coordinate out of the bounds of the chess board"
    Just c ->
      let rowNum = case show $ x + 1 of
            [n] -> n
            _ -> error "invalid row number"
       in [c, rowNum]

-- | Squared distance between two coordinates.
distanceSq :: Coord -> Coord -> Int
distanceSq (x1, y1) (x2, y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
