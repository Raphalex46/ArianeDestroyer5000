{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Functions for the rules of chess
module Chess.Rules (validSquaresFromCoord, validMovesFromCoord, GameError, playMove, isKingInCheckmate, isKingInStalemate, getEndType, WinType (..), EndGameType (..), DrawType (..), getAllValidMoves, unwrapState) where

import Control.Applicative
import Data.List

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces

-- | Datatype describing a game error.
data GameError = GameError deriving (Show)

-- | In case of a draw, this is the reason for the draw.
data DrawType = Stalemate | DeadPosition | Handshake | FiftyMoves | ThreefoldRepetition

-- | In case of the game ending in a win, describes the reason for the win.
data WinType = Checkmate | Resign

-- | Type of endgame situation
data EndGameType
  = -- | In case of a win, contains the color that wins and the reason of the win.
    Win (Color, WinType)
  | -- | In case of a draw, contains the reason for the draw.
    Draw (DrawType)

{- | Returns a list of coordinates representing valid squares the piece at
the given `Coord` can move to
-}
validSquaresFromCoord :: GameState -> Coord -> [Coord]
validSquaresFromCoord GameState{..} coord =
  case board ! coord of
    Empty -> []
    Occ (Piece (col, ty)) ->
      let coords = filter (\c -> let sq = board ! c in not (sq `isPieceType` King || sq `isCol` col)) $ movableSquares board coord
       in case ty of
            Pawn ->
              coords
                ++ maybe
                  [] -- If Nothing, then don't add any coord
                  (\x -> if x `elem` attackedSquares board coord then [x] else []) -- If the pawn attacks an enPassant possbility, add it
                  enPassantCoord
            _ -> coords

{- | Returns a list of possible move that can be legally done with the piece
at the given `Coord`
-}
validMovesFromCoord :: GameState -> Coord -> [Move]
validMovesFromCoord gameState@GameState{castlingRights = CastlingRights rightsF, ..} coord =
  case board ! coord of
    Empty -> []
    Occ (Piece (col, ty)) ->
      let coords = map (MovePiece coord) $ validSquaresFromCoord gameState coord
          candidateMoves = case ty of
            King -> coords ++ map (Castle col) (filter (isCastlePossible board col) $ rightsF col)
            Pawn -> concat $ map (forcePawnPromotion col) coords
            _ -> coords
       in filter (not . moveResultsInCheck) candidateMoves
 where
  -- Function to translate pawn moves to the last rank to pawn promotion
  forcePawnPromotion col move@(MovePiece src dst@(row, _)) =
    if row == lastRank board col
      then
        map (Promote src dst) [Rook, Knight, Bishop, Queen]
      else
        [move]
  forcePawnPromotion _ otherMove = [otherMove]
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
isKingInCheckmate gameState@GameState{..} color =
  (isKingInCheck board color) && (length $ getAllValidMoves gameState color) == 0

-- Get the list of all valid moves for a given `Color`
getAllValidMoves :: GameState -> Color -> [Move]
getAllValidMoves gameState@GameState{..} color =
  let allyPiecesCoord = getSquaresOfCol board color
   in concat $ map (validMovesFromCoord gameState . fst) allyPiecesCoord

-- Checks whether the player of the given `Color` is in a stalemate situation (no moves available)
isKingInStalemate :: GameState -> Color -> Bool
isKingInStalemate gameState@GameState{..} color =
  (not $ isKingInCheck board color) && (length $ getAllValidMoves gameState color) == 0

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
    Just target
      | isPieceType (board ! src) Pawn -> dst == target
      | otherwise -> False
    Nothing -> False
isEnPassant _ _ = False

{- | Given a `Move`, get the possible en passant `Coord` and current position
of the moved pawn if there is an en passant possiblity
-}
getEnPassantCoord :: Board -> Move -> Maybe Coord
getEnPassantCoord board move@(MovePiece (sRow, sCol) (tRow, _)) =
  if isMovePawnTwoRanks board move
    then let dir = signum (tRow - sRow) in Just (sRow + dir, sCol)
    else Nothing
getEnPassantCoord _ _ = Nothing

{- | Apply a move to the board (move / remove / add pieces accordingly)
Note: this function does not check the rules
-}
applyMove :: GameState -> Move -> Either GameError Board
-- Handle normal piece moves
applyMove gameState@GameState{..} move@(MovePiece src dst) =
  case enPassantCoord of
    Nothing -> execMove
    Just (tRow, tCol)
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
applyMove GameState{..} (Castle _ side) =
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
-- Handle pawn promotion
applyMove GameState{..} (Promote src dst pt) =
  case movePiece board src dst of
    Left _ -> Left GameError
    Right b -> Right $ setPiece b dst (Piece (turn, pt))

{- | Returns an updated `CastlingRights` function given the `GameState` and an
applied `Move`.

Following the standard rules of chess, moving the king or castling removes
all castling rights for the corresponding color. Moving a rook removes
castling rights for the color on the side of the moved rook.
-}
updateCastlingRights :: GameState -> Move -> CastlingRights
updateCastlingRights GameState{castlingRights = castlingRights@(CastlingRights rightsF), ..} move =
  case move of
    Castle col _ -> CastlingRights $ removeRights col [QueenSide, KingSide]
    MovePiece src _ -> case (board ! src) of
      Empty -> castlingRights
      Occ (Piece (col, ty)) -> case ty of
        King -> CastlingRights $ removeRights col [QueenSide, KingSide]
        Rook -> CastlingRights $ removeRights col [(getRookSide src)]
        _ -> castlingRights
    _ -> castlingRights
 where
  removeRights col sides c =
    if c == col
      then
        (rightsF c) \\ sides
      else
        rightsF c

-- | Play a move, updating the 'GameState' accordingly
playMove :: GameState -> Move -> Either GameError GameState
playMove gameState@GameState{..} move
  | isCol (board ! src) turn = playMoveCorrectColor
  | otherwise = Left GameError
 where
  playMoveCorrectColor
    | move `elem` (validMovesFromCoord gameState src) =
        case applyMove gameState move of
          Right b ->
            let resetHistory = moveIsCapture board move || moveIsPiece board Pawn move
             in Right
                  gameState
                    { board = b,
                      enPassantCoord = getEnPassantCoord board move,
                      turn = opp turn,
                      castlingRights = updateCastlingRights gameState move,
                      history =
                        if resetHistory
                          then
                            []
                          else
                            -- `:` is faster than `++` so we add the position to the head of the list :)
                            (historyStateFromGameState gameState) : history,
                      halfMoveClock =
                        if resetHistory
                          then 0
                          else case turn of
                            White -> halfMoveClock
                            Black -> halfMoveClock + 1,
                      fullMoveClock = fullMoveClock + 1
                    }
          Left _ -> Left GameError
    | otherwise = Left GameError
  src = getSrcCoord board move

{- | Check if the position is a dead position (insufficient material for both sides).
I'm using the heuristics from chess.com here: https://support.chess.com/en/articles/8705277-what-does-insufficient-mating-material-mean.
This is not complete and does not handle more complicated types of dead positions, but I think it's
sufficient for now.
-}
isDeadPosition :: Board -> Bool
isDeadPosition board =
  all insufficientMaterial allColors
 where
  -- To be safe, just check lone king versus some basic insufficient material configurations.
  insufficientMaterial col =
    let pieces = remainingPieceTypes $ col
     in case remainingPieceTypes $ opp col of
          [King] -> length (filter (\x -> x == King || x == Knight || x == Bishop) pieces) <= 2
          _ -> False
  remainingPieceTypes col =
    foldl
      ( ( \acc x -> case snd x of
            Empty -> acc
            Occ (Piece (_, pt)) -> pt : acc
        )
      )
      []
      $ getSquaresOfCol board col

{- | Get the type of ending for this game state.
If the game is not over yet, returns `Nothing`, otherwise, returns the
reason for the ending of the game
-}
getEndType :: GameState -> Maybe EndGameType
getEndType gs =
  ( find (isKingInCheckmate gs) colors
      >>= (\col -> Just $ Win ((opp col), Checkmate))
  )
    <|> ( find (isKingInStalemate gs) colors
            >>= (\_ -> Just $ Draw (Stalemate))
        )
    <|> ( if halfMoveClock gs >= 100
            then return $ Draw (FiftyMoves)
            else
              Nothing
        )
    <|> ( if (countRep :: Integer) >= 2
            then return $ Draw (ThreefoldRepetition)
            else
              Nothing
        )
    <|> ( if isDeadPosition (board gs)
            then return $ Draw (DeadPosition)
            else
              Nothing
        )
    <|> Nothing
 where
  colors = [Black, White]
  countRep =
    let histState = historyStateFromGameState gs
     in foldl (\cnt x -> if x == histState then cnt + 1 else cnt) 0 $ history gs

{- | A utility function, kind of ugly but useful when
you're certain the wrapped `GameState` will be valid
-}
unwrapState :: Either GameError GameState -> GameState
unwrapState (Right gs) = gs
unwrapState (Left err) = error (show err)
