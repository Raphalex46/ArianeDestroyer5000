{-# LANGUAGE RecordWildCards #-}
module IO.GameState (showState) where

import Chess.GameState
import IO.Board

showState :: GameState -> String
showState GameState {..} =
  showBoard board ++
  "\n" ++
  "Active color: " ++ show turn ++ "\n" ++
  "Turn: " ++ show (fullMoveClock + 1)
