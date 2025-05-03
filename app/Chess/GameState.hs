{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Data structure for the state of the chess game
module Chess.GameState (GameState (..), CastlingRights (..), historyStateFromGameState) where

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.Moves

{- | Castling rights are represented by a function returning a list of side
given a color: The returned list contains the `Side`s on which castling
is possible for the given color.

This is a newtype now because we need to make this an instance of Eq for
checking the threefold repetition rule.
-}
newtype CastlingRights = CastlingRights (Color -> [Side])

instance Eq CastlingRights where
  (==) (CastlingRights a) (CastlingRights b) =
    let colors = [White, Black]
     in map a colors == map b colors

{- | This type is a kind of reduced `GameState` containing only the
information needed to compare gamestates for the sake of checking
threefold repetition.
-}
type HistoryState = (Board, Maybe Coord, CastlingRights)

-- | Type for the state of the game
data GameState = GameState
  { -- | The game board
    board :: Board,
    {- | This is Nothing is the last move was anything but a pawn moving two
    ranks. If the last move is a pawn moving two ranks, this holds the `Coord`
    that can be captured with the enPassant move.
    -}
    enPassantCoord :: Maybe Coord,
    -- | Describes the castling rights for each player (see `CastlingRights`).
    castlingRights :: CastlingRights,
    -- | Current active color.
    turn :: Color,
    -- | Number of half-turns since last pawn move or piece capture.
    halfMoveClock :: Int,
    -- | Number of full-turns since the beginning of the game.
    fullMoveClock :: Int,
    -- | Position history. Used to check the threefold repetition rule.
    history :: [HistoryState]
  }

{- | Create a `HistoryState` from a normal `GameState`.
Just works by keep only the board, en passant coordinates and castling rights.
-}
historyStateFromGameState :: GameState -> HistoryState
historyStateFromGameState GameState{board = b, enPassantCoord = ep, castlingRights = cr} = (b, ep, cr)
