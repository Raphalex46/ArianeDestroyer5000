{-# LANGUAGE RecordWildCards #-}

module IO.GameState (showState, showStateStatus) where

import Chess.GameState
import IO.Board

-- | Print a `GameState`.
showState :: GameState -> String
showState gs@GameState{..} =
  showBoard board
    ++ "\n"
    ++ showStateStatus gs

-- | Show every information except the board from a `GameState`.
showStateStatus :: GameState -> String
showStateStatus GameState{..} =
  "Active color: "
    ++ show turn
    ++ "\n"
    ++ "Turn: "
    ++ show fullMoveClock
    ++ "\n"
    ++ "Turns without take or pawn move: "
    ++ (show $ halfMoveClock)
