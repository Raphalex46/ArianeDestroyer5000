module Main where

import Chess.Record
import Chess.Colors
import Bot
import Cli
import IO.Standard.Loop

main :: IO ()
main = playGame =<< execOptionsParser

playGame :: Options -> IO ()
playGame _ =
    let s = case gameStateFromFENString startingFENString of
            Right g -> g
            Left _ -> error "Failed to parse FEN String"
        config = Config {
          player = (\c -> case c of
                            White -> Human
                            Black -> Bot Random
              )
        }
     in do
            loop config s
