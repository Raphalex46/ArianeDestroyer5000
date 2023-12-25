module Main where

import Chess

interactLoop :: Game -> IO ()
interactLoop game =
  do
    let gBoard = board game
        gTurnNum = turnNum game
        gTurn = turn game
    putStrLn $ showGame game
    move <- fmap fromString getLine
    interactLoop $ game {
    board=playMove gBoard move,
    turn=Chess.cycle gTurn :: Color,
    turnNum=gTurnNum + 1,
    lastMove=Just move
  }

main :: IO ()
main = interactLoop startGame
