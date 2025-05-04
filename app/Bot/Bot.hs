-- | This module contains an enum describing types of bot, and basic functions for using bots.
module Bot.Bot (BotType (..), Bot (..), selectMove) where

import Bot.MinMax
import Bot.Random
import Chess.GameState
import Chess.Moves

-- | Enum describing different kinds of bots.
data BotType = Random | MinMax deriving (Show, Read)

{- | Structure describing a bot.
| Each type of bot carries its own specific state.
-}
data Bot
  = -- | A bot that makes random moves.
    RandomBot RandomBotState
  | -- | A more classical and intelligent bot.
    MinMaxBot MinMaxBotState

{- | The main thing: move selection. This takes a `Bot` and a `GameState` and
returns the selected move as well as a new bot. This function updates the
state of the bot. This means that the new state should be used for further
use.
-}
selectMove :: Bot -> GameState -> (Move, Bot)
selectMove (RandomBot st) gs = let (move, newSt) = selectMoveRandomBot st gs in (move, RandomBot newSt)
selectMove (MinMaxBot st) gs = let (move, newSt) = selectMoveMinMaxBot st gs in (move, MinMaxBot newSt)
