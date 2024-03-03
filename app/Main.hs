module Main where

import Chess.Board
import IO.Modes
import IO.MoveExpression

main :: IO ()
main = do
  let i =
        map
          ( \_ -> do
              putStrLn $ showBoard startingBoard
              readMoveExpression Standard
          )
          [0 .. 10]
  sequence i
  return ()
