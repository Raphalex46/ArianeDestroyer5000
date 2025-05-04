-- | Implementation of a bot making random moves.
module Bot.Random (selectMoveRandomBot, RandomBotState, initRandomBot) where

import Chess.GameState
import Chess.Moves
import Chess.Rules
import System.Random

-- | The state of a random bot.
data RandomBotState = RandomBotState
  { -- | The only element in the random state is the random number generator.
    randomState :: StdGen
  }

-- | Initialize the random bot's state with a random number generator.
initRandomBot :: StdGen -> RandomBotState
initRandomBot gen = RandomBotState{randomState = gen}

-- | Move selection algorithm for our little random bot.
selectMoveRandomBot :: RandomBotState -> GameState -> (Move, RandomBotState)
selectMoveRandomBot botState gs =
  case getAllValidMoves gs (turn gs) of
    [] -> error "unreachable"
    l ->
      let (randInd, newState) = uniformR (0, (length l) - 1) (randomState botState)
       in (l !! randInd, botState{randomState = newState})
