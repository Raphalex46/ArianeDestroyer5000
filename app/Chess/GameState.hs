-- | Data structure for the state of the chess game
module Chess.GameState (GameState (..), startingState, CastlingRights) where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.Moves

type CastlingRights = Color -> [Side]

-- | Type for the state of the game
data GameState = GameState
  { -- | The game board
    board :: Board,
    -- | This is Nothing is the last move was anything but a pawn moving two
    -- ranks. If the last move is a pawn moving two ranks, this holds the `Coord`
    -- that can be captured with the enPassant move in fst, and the position
    -- of the pawn that will be captured in snd.
    enPassantCoord :: Maybe (Coord, Coord),
    castlingRights :: CastlingRights,
    turn :: Color
  }

-- | Returns a standard chess starting state
startingState :: GameState
startingState =
  GameState
    { board = startingBoard,
      enPassantCoord = Nothing,
      castlingRights = (\_ -> [QueenSide, KingSide]),
      turn = White
    }
