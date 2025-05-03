-- | This module contains everything related to chess bots, as well as bot implementations
module Bot (BotType (..), selectMove, PlayerType (..), Bot (..), initRandomBot) where

import Chess.GameState
import Chess.Moves
import Chess.Rules
import System.Random

-- | Type of player: can be either a bot or a human
data PlayerType = Human | Bot BotType deriving (Show, Read)

-- | Enum describing different kinds of bots.
data BotType = Random deriving (Show, Read)

{- | Structure describing a bot.
| Each type of bot carries its own specific state.
-}
data Bot
  = -- | A bot that makes random moves
    RandomBot RandomBotState

-- | The state of a random bot.
data RandomBotState = RandomBotState
  { -- | The only element in the random state is the random number generator.
    randomState :: StdGen
  }

-- | Initialize the random bot's state with a random number generator.
initRandomBot :: StdGen -> Bot
initRandomBot gen = RandomBot $ RandomBotState { randomState = gen }

{- | The main thing: move selection. This takes a `Bot` and a `GameState` and
returns the selected move as well as a new bot. This function updates the
state of the bot. This means that the new state should be used for further
use.
-}
selectMove :: Bot -> GameState -> (Move, Bot)
selectMove (RandomBot botState) gs =
  case getAllValidMoves gs (turn gs) of
    [] -> error "unreachable"
    l ->
        let (randInd, newState) = uniformR (0, (length l) - 1) (randomState botState) in
        (l !! randInd, RandomBot $ botState{randomState = newState})
