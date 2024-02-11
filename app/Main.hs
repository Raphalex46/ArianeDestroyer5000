module Main where

import Chess
import Data.Array

interactLoop :: Game -> IO ()
interactLoop game =
  do
    let gBoard = board game
        gTurnNum = turnNum game
        gTurn = turn game
    putStrLn $ showGame game
    move@(src, _) <- fmap fromString getLine
    if not $ isValid game move
      then do
        putStrLn "Invalid move!"
        interactLoop game
      else
        let isKingMove = isType (gBoard ! src) King
            isRookMove = isType (gBoard ! src) Rook
          in
        interactLoop $
          game
            { board = playMove gBoard move,
              turn = cycleCol gTurn :: Color,
              turnNum = gTurnNum + 1,
              lastMove = Just move,
              canCastle = (\col -> if col == turn game then canCastle game col && (not $ (isKingMove || isRookMove)) else canCastle game col)
            }

main :: IO ()
main = interactLoop startGame
