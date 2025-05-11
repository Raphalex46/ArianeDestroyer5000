module IO.UCI.State (UCIState (..), startingUCIState) where

import Chess.GameState
import Bot.Bot
import Chess.Record

-- | State of the program when running in UCI mode.
data UCIState =
  UCIState {
    gameState :: GameState,
    bot :: Bot
  }

-- | A default starting state for UCI (provided a bot).
startingUCIState :: Bot -> UCIState
startingUCIState bot = UCIState {gameState = getStartingGameState, bot = bot}
