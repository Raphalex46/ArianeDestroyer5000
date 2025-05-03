module Bot (BotType (..), selectMove) where

import Chess.GameState
import Chess.Moves
import Chess.Rules

-- | Enum describing different kinds of bots.
data BotType = Random deriving (Show, Read)

-- | The move selection algorithm for a given bot.
selectMove :: BotType -> GameState -> Move
selectMove Random gs =
    case getAllValidMoves gs (turn gs) of
        m : _ -> m
        [] -> error "unreachable"
