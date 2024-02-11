module Main where

import Chess
import Data.Array

interactLoop :: Game -> IO ()
interactLoop game =
    if isCheckMated game (turn game)
       then
         putStrLn "Checkmate"
       else
        do 
        putStrLn $ showGame game
        move <- fmap fromString getLine
        interactLoop' move
      where
        interactLoop' move@(src, dst)
          | isValid game move =
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
          | otherwise =
            do
              putStrLn "Invalid move!"
              interactLoop game
            where
              gBoard = board game
              gTurnNum = turnNum game
              gTurn = turn game

main :: IO ()
main = interactLoop startGame
