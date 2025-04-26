{-# LANGUAGE RecordWildCards #-}
module IO.GameState (showState) where

import Chess.GameState
import IO.Board

-- | Print a `GameState`.
showState :: GameState -> String
showState GameState {..} =
  showBoard board ++
  "\n" ++
  "Active color: " ++ show turn ++ "\n" ++
  "Turn: " ++ show fullMoveClock ++ "\n" ++
  "Turns without take or pawn move: " ++ (show $ length history)
