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
    if not $ isValid game move
      then do
        putStrLn "Invalid move!"
        interactLoop game
      else
        interactLoop $
          game
            { board = playMove gBoard move,
              turn = cycleCol gTurn :: Color,
              turnNum = gTurnNum + 1,
              lastMove = Just move,
              kingChecked = whoIsChecked game
            }

main :: IO ()
main = interactLoop startGame
