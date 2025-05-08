{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Implementation of a minmax algorithm
module Bot.MinMax where

import Chess.Board
import Chess.Colors
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces
import Chess.Rules

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
  let move = snd $ minmax gs 5 (-inf) (inf)
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
          [] -> error "No valid move!"
          firstMove:_ -> getBestMove (n-1) a b (minScore turn) firstMove gs moves
    in (score, return move)

-- | Find the best move by running minmax on the child moves.
-- Performs alpha beta pruning (this is why I implement it as a recursive
-- function as we need to be able to cutoff the search
getBestMove :: Int -> Score -> Score -> Score -> Move -> GameState -> [Move] -> (Score, Move)
getBestMove _ _ _ res mov gs@GameState{turn=turn} [] = (res, mov)
getBestMove n a b res mov gs@GameState{turn=turn} (curMove:ps)
  | cutoff turn a b res = (res, mov)
  | otherwise = let newAlpha = (updateAlpha turn a res)
                    newBeta = (updateBeta turn b res)
                    curState = unwrapState . playMove gs $ curMove
                    (newRes, _) = minmax curState n newAlpha newBeta
                    (bestRes, bestMove) = if (comp turn) newRes res then (newRes, curMove) else (res, mov)
                     in getBestMove n newAlpha newBeta bestRes bestMove gs ps

-- | Evaluation function a board.
eval :: GameState -> Score
eval gs =
  let mat = materialValue gs White - materialValue gs Black
      win = checkValue gs
      mob = mobilityValue gs White - mobilityValue gs Black
      con = controlValue gs White - controlValue gs Black
   in -- This is very much random heuristics lol
      mat + win + con

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

-- | Value for checks and checkmates.
checkValue :: GameState -> Score
checkValue gs@GameState{board = board}
  | isKingInCheck' Black = 0.2
  | isKingInCheck' White = -0.2
  | isKingInCheckmate' Black = inf
  | isKingInCheckmate' White = -inf
  | otherwise = 0.0
 where
  isKingInCheck' = isKingInCheck board
  isKingInCheckmate' = isKingInCheckmate gs

-- | Value for the control of squares on the board.
controlValue :: GameState -> Color -> Score
controlValue GameState{board = board} col =
  sum . map (\(x, y) -> if x `elem` [3, 4] && y `elem` [3,4] then 0.1 else 0.05) $ attackedSquaresByColor board col

mobilityValue :: GameState -> Color -> Score
mobilityValue gs col =
  (fromIntegral . length $ getAllValidMoves gs col) / 20

-- | Returns the appropriate comparison function according to the color
-- (whether we want to maximize or minimize score)
comp :: Ord a => Color -> a -> a -> Bool
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

-- | Returns if we should cutoff the search, given alpha,
-- beta, and the current score
cutoff :: Color -> Score -> Score -> Score -> Bool
cutoff White _ b res = res > b
cutoff Black a _ res = res < a

{- | Unwrap a state from an either. Crash if an error value is encountered.
The error case should never occur because we only get boards by playing
valid moves (played with the `getAllValidMoves` function). If an error
occurs here, that means there is an implementation error somewhere in the game.
-}
unwrapState :: Either GameError GameState -> GameState
unwrapState (Right gs) = gs
unwrapState (Left err) = error (show err)
