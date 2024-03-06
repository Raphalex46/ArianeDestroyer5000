{-# LANGUAGE RecordWildCards #-}

-- | Functions for the rules of chess
module Chess.Rules (validSquaresFromCoord, validMovesFromCoord, GameError, playMove) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces

data GameError = GameError deriving(Show)

-- | Returns a list of coordinates representing valid squares the piece at
-- the given `Coord` can move to
validSquaresFromCoord :: GameState -> Coord -> [Coord]
validSquaresFromCoord GameState {..} coord =
  case board ! coord of
    Empty -> []
    Occ (Piece (col, ty)) ->
      let coords = filter (not . (flip isCol) col . (board !)) $ movableSquares board coord
      in case ty of
          Pawn -> coords -- Add EnPassant possibility
          King -> coords -- Add Castle possibility
          Rook -> coords -- Add Castle possibility
          _ -> coords

-- | Returns a list of possible move that can be legally done with the piece
-- at the given `Coord`
validMovesFromCoord :: GameState -> Coord -> [Move]
validMovesFromCoord game coord = map (MovePiece coord) $ validSquaresFromCoord game coord

playMove :: GameState -> Move -> Either GameError GameState
playMove gameState@GameState {..} move@(MovePiece src dst) 
  | move `elem` (validMovesFromCoord gameState src) = case movePiece board src dst of
    Right b -> Right GameState{board=b}
    Left _ -> Left GameError
  | otherwise = Left GameError
playMove _ _ = error "This kind of move is not yet implemented"
