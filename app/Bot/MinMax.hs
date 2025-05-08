{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

-- | Implementation of a minmax algorithm
module Bot.MinMax where

import Chess.Board
import Chess.Colors
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces
import Chess.Rules
import Data.List
import Chess.Coord

{- | To keep things simple with the general `Bot` interface,
we define a state even if we don't use it. It might be useful
to store information later without the need to change the whole
interface each time.
-}
data MinMaxBotState = MinMaxBotState

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
  let move = snd $ minmax gs 4 (-inf) (inf)
   in case move of
        Nothing -> error "No move found??"
        Just m -> (m, st)

-- | Minmax algorithm implementation.
minmax :: GameState -> Int -> Score -> Score -> (Score, Maybe Move)
minmax gs 0 a b = (eval gs, Nothing)
minmax gs@GameState{turn = turn} n a b =
  let moves = getAllValidMoves gs turn
      (score, move) =
        case moves of
          [] -> (0.0, Nothing)
          firstMove : _ -> let (s, m) = getBestMove (n - 1) a b (minScore turn) firstMove gs (orderMoves gs moves) in (s, return m)
   in (score, move)

{- | Find the best move by running minmax on the child moves.
Performs alpha beta pruning (this is why I implement it as a recursive
function as we need to be able to cutoff the search
-}
getBestMove :: Int -> Score -> Score -> Score -> Move -> GameState -> [Move] -> (Score, Move)
getBestMove _ _ _ res mov gs@GameState{turn = turn} [] = (res, mov)
getBestMove n a b res mov gs@GameState{turn = turn} (curMove : ps)
  | cutoff turn a b res = (res, mov)
  | otherwise =
      let newAlpha = (updateAlpha turn a res)
          newBeta = (updateBeta turn b res)
          curState = unwrapState . playMove gs $ curMove
          (newRes, _) = minmax curState n newAlpha newBeta
          (bestRes, bestMove) = if (comp turn) newRes res then (newRes, curMove) else (res, mov)
       in getBestMove n newAlpha newBeta bestRes bestMove gs ps

-- | Evaluation function a board.
eval :: GameState -> Score
eval gs =
  let
      matWhite = materialValue gs White
      matBlack = materialValue gs Black
      mat = matWhite - matBlack
      win = winValue gs
      check = checkValue gs
      mob = mobilityValue gs White - mobilityValue gs Black
      con = (controlValue gs White - matWhite * 2.0) - (controlValue gs Black - matBlack * 2.0)
   in -- This is very much random heuristics lol
      mat + win + 0.2 * check + con + mob

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
checkValue gs@GameState{board = board}
  | isKingInCheck board Black = 1.0
  | isKingInCheck board White = -1.0
  | otherwise = 0.0

-- | Value associated to checkmates.
winValue :: GameState -> Score
winValue gs@GameState{board=board}
  | isKingInCheckmate gs Black = inf
  | isKingInCheckmate gs White = -inf
  | otherwise = 0.0


-- | Value for the control of squares on the board.
controlValue :: GameState -> Color -> Score
controlValue GameState{board = board} col =
  sum . map (\(x, y) -> if x `elem` [3, 4] && y `elem` [3, 4] then 1.0 else 0.5) $ attackedSquaresByColor board col

-- | Mobility = number of available moves
mobilityValue :: GameState -> Color -> Score
mobilityValue gs col =
  fromIntegral . length $ getAllValidMoves gs col

{- | Returns the appropriate comparison function according to the color
(whether we want to maximize or minimize score)
-}
comp :: (Ord a) => Color -> a -> a -> Bool
comp White = (>=)
comp Black = (<=)

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
orderMoves gs@GameState{..} =
  sortBy sortFunc
 where
  sortFunc a b
    | moveIsCapture board a && not (moveIsCapture board b) = LT
    | otherwise = let oppKingPos = getKingCoord board (opp turn)
                      dstA = getDstCoord board a
                      dstB = getDstCoord board b
                  in compare (distanceSq dstA oppKingPos) (distanceSq dstB oppKingPos)

{- | Unwrap a state from an either. Crash if an error value is encountered.
The error case should never occur because we only get boards by playing
valid moves (played with the `getAllValidMoves` function). If an error
occurs here, that means there is an implementation error somewhere in the game.
-}
unwrapState :: Either GameError GameState -> GameState
unwrapState (Right gs) = gs
unwrapState (Left err) = error (show err)
