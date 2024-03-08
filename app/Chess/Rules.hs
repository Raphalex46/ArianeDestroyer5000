{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
          Pawn ->
            coords ++ maybe
              [] -- If Nothing, then don't add any coord
              (\(x, _) -> if x `elem` attackedSquares board coord then [x] else []) -- If the pawn attacks an enPassant possbility, add it
              enPassantCoord
          King -> coords -- Add Castle possibility
          Rook -> coords -- Add Castle possibility
          _ -> coords

-- | Returns a list of possible move that can be legally done with the piece
-- at the given `Coord`
validMovesFromCoord :: GameState -> Coord -> [Move]
validMovesFromCoord game coord = map (MovePiece coord) $ validSquaresFromCoord game coord

-- Various predicates on moves

-- The move is a pawn move of 2 ranks
isMovePawnTwoRanks :: Board -> Move -> Bool
isMovePawnTwoRanks board (MovePiece src@(sRow, _) (tRow, _)) =
  (isPieceType (board ! src) Pawn) && (abs $ tRow - sRow) == 2
isMovePawnTwoRanks _ _ = False

-- | The move is an en passant move
isEnPassant :: GameState -> Move -> Bool
isEnPassant GameState{..} (MovePiece src dst) =
  case enPassantCoord of
    Just (target, _)
        | isPieceType (board ! src) Pawn -> dst == target
        | otherwise -> False
    Nothing -> False
isEnPassant _ _ = False

-- | Given a `Move`, get the possible en passant `Coord` and current position
-- of the moved pawn if there is an en passant possiblity
getEnPassantCoord :: Board -> Move -> Maybe (Coord, Coord)
getEnPassantCoord board move@(MovePiece (sRow, sCol)  (tRow, tCol)) =
  if isMovePawnTwoRanks board move then
    let dir = signum (tRow - sRow) in Just ((sRow + dir, sCol), (tRow, tCol))
  else Nothing
getEnPassantCoord _ _ = Nothing

-- | Apply a move to the board (move / remove / add pieces accordingly)
applyMove :: GameState -> Move -> Either GameError Board
applyMove gameState@GameState {..} move@(MovePiece src dst) =
  case enPassantCoord of
    Nothing -> execMove
    Just (_, target)
      | isEnPassant gameState move -> execMove >>= (\x -> return $ removePiece x target)
      | otherwise -> execMove
  where
    execMove = case movePiece board src dst of
      Left _ -> Left GameError
      Right board -> Right board
applyMove _ _ = error "Not yet implemented"


-- | Play a move, updating the 'GameState' accordingly
playMove :: GameState -> Move -> Either GameError GameState
playMove gameState@GameState {..} move@(MovePiece src _)
  | move `elem` (validMovesFromCoord gameState src) = case applyMove gameState move of
    Right b -> Right GameState{board=b, enPassantCoord=getEnPassantCoord board move}
    Left _ -> Left GameError
  | otherwise = Left GameError
playMove _ _ = error "This kind of move is not yet implemented"
