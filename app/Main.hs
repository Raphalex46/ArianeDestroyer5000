module Main where

import Chess.Record
import Chess.Colors
import Bot
import Cli
import IO.Standard.Loop
import IO.GameState

main :: IO ()
main = playGame =<< execOptionsParser

playGame :: Options -> IO ()
playGame opts =
    let s = case gameStateFromFENString startingFENString of
            Right g -> g
            Left _ -> error "Failed to parse FEN String"
        config = Config {
          player = (\c -> case c of
                            White -> (whitePlayer opts)
                            Black -> (blackPlayer opts)
              )
        }
     in do
            loop config s
