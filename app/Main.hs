module Main where

import Chess.GameState
import IO.Board
import IO.Standard.Loop

main :: IO ()
main =
  let s = startingState
  in do
    putStrLn $ showBoard (board s)
    loop startingState
