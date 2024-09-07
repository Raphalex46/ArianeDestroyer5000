{-# LANGUAGE RecordWildCards #-}
-- | Data structure for the state of the chess game
module Chess.GameState (GameState (..), CastlingRights) where

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
    -- that can be captured with the enPassant move.
    enPassantCoord :: Maybe Coord,
    -- | Describes the castling rights for each player (see `CastlingRights`).
    castlingRights :: CastlingRights,
    -- | Current active color.
    turn :: Color,
    -- | Number of half-turns since last pawn move or piece capture.
    halfMoveClock :: Int,
    -- | Number of full-turns since the beginning of the game.
    fullMoveClock :: Int
  }
