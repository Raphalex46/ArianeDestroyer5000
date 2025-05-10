module IO.UCI.State (UCIState (..)) where

import Chess.GameState

data UCIState = Handshake | Loop GameState
