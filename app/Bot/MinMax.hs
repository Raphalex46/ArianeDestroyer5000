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
import Control.Parallel.Strategies
import Data.Foldable

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
selectMoveMinMaxBot st gs@GameState{turn = turn} =
  let possibleStates = (\x -> ((unwrapState . playMove gs) x, x)) <$> getAllValidMoves gs turn
      -- Run the minmax on each possible move, and get back the move that works best
      (_, move) = (optBy turn) (\a b -> compare (fst a) (fst b)) $ (\(a, b) -> (minmax a 3, b)) <$> possibleStates
   in (move, st)

-- | Minmax algorithm implementation.
minmax :: GameState -> Int -> Score
minmax gs 0 = eval gs
minmax gs@GameState{turn = turn} n =
  let possibleStates = (\x -> (unwrapState . playMove gs) x) <$> getAllValidMoves gs turn
      scores = parMap rdeepseq (\s -> minmax s (n - 1)) possibleStates
      best = case scores of
        [] -> minScore turn
        s -> (optBy turn) compare $ s
   in best

-- | Evaluation function a board.
eval :: GameState -> Score
eval gs =
  let mat = materialValue gs White - materialValue gs Black
      win = checkValue gs
      con = controlValue gs White - controlValue gs Black
   in -- This is very much random heuristics lol
      mat * win + con

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
  | isKingInCheck' Black = 2.0
  | isKingInCheck' White = -2.0
  | isKingInCheckmate' Black = inf
  | isKingInCheckmate' White = -inf
  | otherwise = 0.0
 where
  isKingInCheck' = isKingInCheck board
  isKingInCheckmate' = isKingInCheckmate gs

-- | Value for the control of squares on the board.
controlValue :: GameState -> Color -> Score
controlValue GameState{board = board} col =
  let controlled = fromIntegral . length $ attackedSquaresByColor board col
   in controlled / 10

{- | Optimization function (either max or min), return in function of the
`Color` (`maximum` for `White`, `minimum` for `Black`)
-}
optBy :: (Foldable t) => Color -> (a -> a -> Ordering) -> t a -> a
optBy Black = minimumBy
optBy White = maximumBy

{- | Unwrap a state from an either. Crash if an error value is encountered.
The error case should never occur because we only get boards by playing
valid moves (played with the `getAllValidMoves` function). If an error
occurs here, that means there is an implementation error somewhere in the game.
-}
unwrapState :: Either GameError GameState -> GameState
unwrapState (Right gs) = gs
unwrapState (Left err) = error (show err)
