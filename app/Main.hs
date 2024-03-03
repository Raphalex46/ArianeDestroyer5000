module Main where

import Chess.Board

main :: IO ()
main = putStrLn $ showBoard startingBoard
