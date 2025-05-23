module Main where

import Bot.Bot
import Bot.MinMax
import Bot.Random
import Chess.Colors
import Chess.Record
import Cli
import IO.GameState
import IO.Standard.Loop
import IO.Standard.ProgramState
import Player
import System.Random

main :: IO ()
main = playGame =<< execOptionsParser

-- | Entry point for the game. Takes the parsed command line options as an argument.
playGame :: Options -> IO ()
playGame opts =
  do
    gen <- newStdGen
    let
      wp = whitePlayer opts
      bp = blackPlayer opts
      s = case gameStateFromFENString startingFENString of
        Right g -> g
        Left _ -> error "Failed to parse FEN String"
      progState =
        defaultProgState
          { bots =
              ( \c -> case c of
                  White -> botFromPlayerType gen wp
                  Black -> botFromPlayerType gen bp
              ),
            uciBot = case botFromPlayerType gen $ Bot (uciBotType opts) of
              Nothing -> error "Invalid bot for UCI"
              Just r -> r
          }
     in
      do
        putStrLn $ showState s
        loop progState
 where
  botFromPlayerType _ Human = Nothing
  botFromPlayerType gen (Bot Random) = return . RandomBot $ initRandomBot gen
  botFromPlayerType _ (Bot MinMax) = return . MinMaxBot $ initMinMaxBot
