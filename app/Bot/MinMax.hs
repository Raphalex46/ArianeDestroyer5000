{-# LANGUAGE RecordWildCards #-}

-- | Implementation of a minmax algorithm
module Bot.MinMax where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces
import Chess.Rules
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List
import Data.List.Split

{- | To keep things simple with the general `Bot` interface,
we define a state even if we don't use it. It might be useful
to store information later without the need to change the whole
interface each time.
-}
data MinMaxBotState = MinMaxBotState

-- | Input parameters to a search.
data SearchIn = SearchIn
  { depth :: Int,
    alpha :: Score,
    beta :: Score,
    gameState :: GameState
  }

-- | Output of a search.
data SearchOut = SearchOut
  { bestScore :: Score,
    bestMove :: Move
  }

instance NFData SearchOut where
  rnf a = seq a ()

-- | The score to give to a board.
type Score = Double

inf :: Score
inf = 1 / 0

-- | Initialize the `MinMaxBot`'s state.
initMinMaxBot :: MinMaxBotState
initMinMaxBot = MinMaxBotState

-- | Minimum score for a color (-inf or inf).
minScore :: Color -> Score
minScore White = -inf
minScore Black = inf

-- | Maxiumum score for a color (-inf or inf).
maxScore :: Color -> Score
maxScore White = inf
maxScore Black = -inf

-- | Move selection algorithm for the minmax bot.
selectMoveMinMaxBot :: MinMaxBotState -> GameState -> (Move, MinMaxBotState)
selectMoveMinMaxBot st gs =
  let (firstMove, movesToSearch) = case getAllValidMoves gs (turn gs) of
        [] -> error "No valid move"
        moves@(m : _) -> (m, moves)
      SearchOut{bestMove = move} =
        minmax (turn gs) $
          parMap
            rdeepseq
            ( ( search
                  SearchIn{depth = 3, alpha = (-inf), beta = inf, gameState = gs}
                  SearchOut{bestMove = firstMove, bestScore = minScore (turn gs)}
              )
                . orderMoves gs
            )
            (chunksOf (length movesToSearch `div` 1) movesToSearch)
   in (move, st)

-- | Minmax with alpha-beta pruning search function.
search :: SearchIn -> SearchOut -> [Move] -> SearchOut
search _ searchOut [] = searchOut
search searchIn@SearchIn{gameState = gs@GameState{turn = rootTurn}, ..} searchOut@SearchOut{..} (m : ms)
  | cutoff rootTurn alpha beta bestScore = searchOut
  | otherwise =
      -- Consider depth 1 to be the base case
      let moveResult =
            if depth == 1
              then
                SearchOut{bestMove = m, bestScore = eval . (unwrapState . playMove gs) $ m}
              else
                let curState = unwrapState . playMove gs $ m
                    movesToSearch = getAllValidMoves curState (turn curState)
                 in case movesToSearch of
                      -- We hit a terminal node!
                      [] ->
                        let newScore = case getEndType curState of
                              Nothing -> error "no possible moves but game is not ended?"
                              Just (Win (col, _)) -> case col of
                                Black -> -inf
                                White -> inf
                              Just (Draw _) -> 0.0
                         in searchOut{bestScore = newScore}
                      moves@(fm : _) -> search searchIn{depth = depth - 1, gameState = curState} SearchOut{bestMove = fm, bestScore = minScore (turn curState)} moves
       in search (updateSearchIn searchIn moveResult) (updateSearchOut searchOut moveResult) ms
 where
  updateSearchIn oldIn@SearchIn{alpha = oldAlpha, beta = oldBeta} SearchOut{bestScore = resScore} =
    oldIn{alpha = updateAlpha rootTurn oldAlpha resScore, beta = updateBeta rootTurn oldBeta resScore}
  updateSearchOut oldOut@SearchOut{bestScore = oldBest} SearchOut{bestScore = newBest}
    | (comp rootTurn) oldBest newBest = oldOut
    | otherwise = SearchOut{bestScore = newBest, bestMove = m}

-- | Evaluation function a board.
eval :: GameState -> Score
eval gs =
  let
    matWhite = materialValue gs White
    matBlack = materialValue gs Black
    mat = matWhite - matBlack
    check = checkValue gs
    mob = mobilityValue gs White - mobilityValue gs Black
    con = controlValue gs White - controlValue gs Black
   in
    -- This is very much random heuristics lol
    case getEndType gs of
      Nothing -> 100 * mat + check + con + mob
      Just (Win (col, _)) -> maxScore col
      Just (Draw _) -> 0.0

-- | Point value for each piece.
pieceValue :: PieceType -> Score
pieceValue Pawn = 1.0
pieceValue Knight = 3.0
pieceValue Bishop = 3.0
pieceValue Rook = 5.0
pieceValue Queen = 9.0
pieceValue King = 0.0

-- | The generalized value for a square.
squareValue :: Square -> Score
squareValue Empty = 0.0
squareValue (Occ (Piece (_, pt))) = pieceValue pt

-- | Total material value of a given `Color` (sum of values of pieces).
materialValue :: GameState -> Color -> Score
materialValue GameState{board = board} col =
  sum $ squareValue . snd <$> getSquaresOfCol board col

-- | Value associated to checks.
checkValue :: GameState -> Score
checkValue GameState{board = board}
  | isKingInCheck board Black = 1.0
  | isKingInCheck board White = -1.0
  | otherwise = 0.0

-- | Value associated to checkmates.
winValue :: GameState -> Score
winValue gs
  | isKingInCheckmate gs Black = inf
  | isKingInCheckmate gs White = -inf
  | otherwise = 0.0

-- | Value for the control of squares on the board.
controlValue :: GameState -> Color -> Score
controlValue GameState{board = board} col =
  let pieces = getSquaresOfCol board col
   in sum $ map normalizedControlScore pieces
 where
  normalizedControlScore (c, sq) =
    case sq of
      Empty -> 0.0
      Occ (Piece (_, pt)) ->
        (sum $ map weightedSquares (attackedSquares board c)) / (maxControl pt * 3.0)
  weightedSquares c =
    1.0 / (distanceToCenter c)
  maxControl Knight = 8.0
  maxControl Pawn = 2.0
  maxControl Queen = 27.0
  maxControl Rook = 14.0
  maxControl Bishop = 13.0
  maxControl King = 8

-- | Mobility = number of available moves
mobilityValue :: GameState -> Color -> Score
mobilityValue gs col =
  sum $ map (toVal . canMove) (getSquaresOfCol (board gs) col)
 where
  toVal True = 1.0
  toVal False = 0.0
  canMove (c, sq) =
    case validMovesFromCoord gs c of
      [] -> False
      moves -> case sq of
        Occ (Piece (_, Knight)) -> (length moves) > 2
        Occ (Piece (_, Bishop)) -> (length moves) > 5
        _ -> True

{- | Returns the appropriate comparison function according to the color
(whether we want to maximize or minimize score)
-}
comp :: (Ord a) => Color -> a -> a -> Bool
comp White = (>=)
comp Black = (<=)

minmax :: Color -> [SearchOut] -> SearchOut
minmax col = (optBy col) (\a b -> compare (bestScore a) (bestScore b))
 where
  optBy White = maximumBy
  optBy Black = minimumBy

-- | Update the alpha value.
updateAlpha :: Color -> Score -> Score -> Score
updateAlpha White a s = max a s
updateAlpha Black a _ = a

-- | Update the beta value.
updateBeta :: Color -> Score -> Score -> Score
updateBeta White b _ = b
updateBeta Black b s = min b s

{- | Returns if we should cutoff the search, given alpha,
beta, and the current score
-}
cutoff :: Color -> Score -> Score -> Score -> Bool
cutoff White _ b res = res > b
cutoff Black a _ res = res < a

-- | Move ordering function to improve alpha-beta pruning.
orderMoves :: GameState -> [Move] -> [Move]
orderMoves GameState{..} =
  sortBy sortFunc
 where
  sortFunc a b
    | moveIsCapture board a && not (moveIsCapture board b) = LT
    | otherwise =
        let oppKingPos = getKingCoord board (opp turn)
            dstA = getDstCoord board a
            dstB = getDstCoord board b
         in compare (distanceSq dstA oppKingPos) (distanceSq dstB oppKingPos)
