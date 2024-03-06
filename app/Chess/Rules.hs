{-# LANGUAGE RecordWildCards #-}

-- | Functions for the rules of chess
module Chess.Rules (validSquaresFromCoord, validMovesFromCoord) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces

-- | Returns a list of coordinates representing valid squares the piece at
-- the given `Coord` can move to
validSquaresFromCoord :: GameState -> Coord -> [Coord]
validSquaresFromCoord GameState {..} coord =
  case board ! coord of
    Empty -> []
    Occ (Piece (col, _)) -> filter (not . (flip isCol) col . (board !)) $ attackedSquares board coord

-- | Returns a list of possible move that can be legally done with the piece
-- at the given `Coord`
validMovesFromCoord :: GameState -> Coord -> [Move]
validMovesFromCoord game coord = map (MovePiece coord) $ validSquaresFromCoord game coord
