module Chess.Coord (Coord, parseCoord) where

import Data.Char
import Data.List
import Text.Read

type Coord = (Int, Int)

colToLet :: Int -> Maybe Char
colToLet i
  | 0 <= i && i <= 7 = find (\x -> ord x == i - ord 'A') ['A' .. 'H']
  | otherwise = Nothing

letToCol :: Char -> Maybe Int
letToCol c
  | toUpper c `elem` ['A' .. 'H'] = Just $ ord (toUpper c) - ord 'A'
  | otherwise = Nothing

parseCoord :: String -> Maybe Coord
parseCoord [c, r] =
  do
    col <- letToCol c
    row <- readMaybe [r]
    -- Similar error handling for columns is taken care of in 'letToCol'
    if 1 <= row && row <= 8
      then Just (row - 1, col)
      else Nothing
parseCoord _ = Nothing
