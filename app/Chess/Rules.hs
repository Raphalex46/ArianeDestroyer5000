{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Functions for the rules of chess
module Chess.Rules (validSquaresFromCoord, validMovesFromCoord, GameError, playMove, isKingInCheckmate) where

import Data.List

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces

data GameError = GameError deriving (Show)

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
              coords
                ++ maybe
                  [] -- If Nothing, then don't add any coord
                  (\x -> if x `elem` attackedSquares board coord then [x] else []) -- If the pawn attacks an enPassant possbility, add it
                  enPassantCoord
            _ -> coords

-- | Returns a list of possible move that can be legally done with the piece
-- at the given `Coord`
validMovesFromCoord :: GameState -> Coord -> [Move]
validMovesFromCoord gameState@GameState {..} coord =
  case board ! coord of
    Empty -> []
    Occ (Piece (col, ty)) ->
      let coords = map (MovePiece coord) $ validSquaresFromCoord gameState coord
          candidateMoves = case ty of
            King -> coords ++ map (Castle col) (filter (isCastlePossible board col) $ castlingRights col)
            _ -> coords
       in filter (not . moveResultsInCheck) candidateMoves
  where
    moveResultsInCheck move =
      let tempBoard = applyMove gameState move
       in case tempBoard of
            Right newBoard -> isKingInCheck newBoard turn
            -- Just consider errors from the `applyMove` function as resulting
            -- in a check (we don't want to include such moves in the final
            -- result)
            Left _ -> True

-- Checks whether the king of the given `Color` is in checkmate or not
isKingInCheckmate :: GameState -> Color -> Bool
isKingInCheckmate gameState@GameState {..} color =
  let allyPiecesCoord = getSquaresOfCol board color
  in (isKingInCheck board color) && (all null $ map (validMovesFromCoord gameState . fst) allyPiecesCoord)

-- Various predicates on moves

-- The move is a pawn move of 2 ranks
isMovePawnTwoRanks :: Board -> Move -> Bool
isMovePawnTwoRanks board (MovePiece src@(sRow, _) (tRow, _)) =
  (isPieceType (board ! src) Pawn) && (abs $ tRow - sRow) == 2
isMovePawnTwoRanks _ _ = False

-- | The move is an en passant move
isEnPassant :: GameState -> Move -> Bool
isEnPassant GameState {..} (MovePiece src dst) =
  case enPassantCoord of
    Just target
      | isPieceType (board ! src) Pawn -> dst == target
      | otherwise -> False
    Nothing -> False
isEnPassant _ _ = False

-- | Given a `Move`, get the possible en passant `Coord` and current position
-- of the moved pawn if there is an en passant possiblity
getEnPassantCoord :: Board -> Move -> Maybe Coord
getEnPassantCoord board move@(MovePiece (sRow, sCol) (tRow, tCol)) =
  if isMovePawnTwoRanks board move
    then let dir = signum (tRow - sRow) in Just (sRow + dir, sCol)
    else Nothing
getEnPassantCoord _ _ = Nothing

-- | Apply a move to the board (move / remove / add pieces accordingly)
-- Note: this function does not check the rules
applyMove :: GameState -> Move -> Either GameError Board
-- Handle normal piece moves
applyMove gameState@GameState {..} move@(MovePiece src dst) =
  case enPassantCoord of
    Nothing -> execMove
    Just target@(tRow, tCol)
      | isEnPassant gameState move -> execMove >>= (\x -> return $ removePiece x pawnCoord)
      | otherwise -> execMove
      where
        pawnCoord = case turn of
                        White -> (tRow - 1, tCol)
                        Black -> (tRow + 1, tCol)
  where
    execMove = case movePiece board src dst of
      Left _ -> Left GameError
      Right board -> Right board
-- Handle castling
applyMove GameState {..} (Castle color side) =
  let kingPos@(kingRow, kingCol) = getKingCoord board turn
      rookPos = castleRookPos board turn side
      direction = case side of
        QueenSide -> -1
        KingSide -> 1
      newKingCol = kingCol + direction * 2
   in either (\_ -> Left GameError) Right $ do
        moveKingBoard <- movePiece board kingPos (kingRow, newKingCol)
        moveRookBoard <- movePiece moveKingBoard rookPos (kingRow, newKingCol - direction)
        return moveRookBoard

-- | Returns an updated `CastlingRights` function given the `GameState` and an
-- applied `Move`.
--
-- Following the standard rules of chess, moving the king or castling removes
-- all castling rights for the corresponding color. Moving a rook removes
-- castling rights for the color on the side of the moved rook.
updateCastlingRights :: GameState -> Move -> CastlingRights
updateCastlingRights GameState {..} move =
  case move of
    Castle col _ -> removeRights col [QueenSide, KingSide]
    MovePiece src _ -> case (board ! src) of
      Empty -> castlingRights
      Occ (Piece (col, ty)) -> case ty of
        King -> removeRights col [QueenSide, KingSide]
        Rook -> removeRights col [(getRookSide src)]
        _ -> castlingRights
    _ -> castlingRights
  where
    removeRights col sides c =
      if c == col then
        (castlingRights c) \\ sides
      else
        castlingRights c
      

-- | Play a move, updating the 'GameState' accordingly
playMove :: GameState -> Move -> Either GameError GameState
playMove gameState@GameState {..} move
  | isCol (board ! src) turn = playMoveCorrectColor
  | otherwise = Left GameError
  where
    playMoveCorrectColor
      | move `elem` (validMovesFromCoord gameState src) =
          case applyMove gameState move of
            Right b ->
              Right
                gameState
                  { board = b,
                    enPassantCoord = getEnPassantCoord board move,
                    turn = opp turn,
                    castlingRights = updateCastlingRights gameState move
                  }
            Left _ -> Left GameError
      | otherwise = Left GameError
    src = getSrcCoord board move
