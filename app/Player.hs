-- | This module contains definitions for different types of player (mainly human and bot players).
module Player (PlayerType (..)) where

import Bot.Bot

-- | Type of player: can be either a bot or a human
data PlayerType = Human | Bot BotType deriving (Show, Read)
