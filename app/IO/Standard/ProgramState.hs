{-# LANGUAGE RecordWildCards #-}

{- | Module for a program state: a game state plus additionnal information
for the standard loop and terminal display.
-}
module IO.Standard.ProgramState (ProgramState (..), defaultProgState) where

import Bot.Bot
import Chess.Colors
import Chess.Coord
import Chess.GameState
import Chess.Record
import IO.Board
import IO.GameState
import qualified System.Console.ANSI as ANSI

-- | A configuration for the game: which players are humans or bots
data ProgramState = ProgramState
  { bots :: (Color -> Maybe Bot),
    lastMove :: Maybe (Coord, Coord),
    game :: GameState,
    coloredSquares :: [(Coord, ANSI.Color)]
  }

-- | A default configuration.
defaultProgState :: ProgramState
defaultProgState =
  ProgramState
    { bots = (\_ -> Nothing),
      lastMove = Nothing,
      game = getStartingGameState,
      coloredSquares = []
    }

instance Show ProgramState where
  show ProgramState{..} =
    let squares = case lastMove of
          Nothing -> []
          Just (a, b) -> map (\x -> (x, ANSI.Black)) [a, b]
        coloredBoard = colorSquares (toColoredBoard $ board game) (coloredSquares ++ squares)
     in showColoredBoard coloredBoard ++ "\n" ++ showStateStatus game
