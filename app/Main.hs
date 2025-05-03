module Main where

import Bot
import Chess.Colors
import Chess.Record
import Cli
import IO.GameState
import IO.Standard.Loop

main :: IO ()
main = playGame =<< execOptionsParser

-- | Entry point for the game. Takes the parsed command line options as an argument.
playGame :: Options -> IO ()
playGame opts =
  let
    wp = whitePlayer opts
    bp = blackPlayer opts
    s = case gameStateFromFENString startingFENString of
      Right g -> g
      Left _ -> error "Failed to parse FEN String"
    config =
      Config
        { bots =
            ( \c -> case c of
                White -> botFromPlayerType wp
                Black -> botFromPlayerType bp
            )
        }
   in
    do
      putStrLn $ showState s
      loop config s
