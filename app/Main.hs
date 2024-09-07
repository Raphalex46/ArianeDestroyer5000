module Main where

import Chess.Record
import IO.GameState
import IO.Standard.Loop

main :: IO ()
main =
   let s = case gameStateFromFENString startingFENString of
             Right g -> g
             Left _ -> error "Failed to parse FEN String"
   in do
        putStrLn $ showState s
        loop s
