-- | Data structure for the state of the chess game
module Chess.GameState (GameState (..)) where

import Chess.Board

-- | Type for the state of the game
data GameState = GameState
  { -- | The game board
    board :: Board
  }
