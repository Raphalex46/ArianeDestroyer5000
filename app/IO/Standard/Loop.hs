{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

import Bot.Bot
import Chess.GameAnalysis
import Chess.GameState
import Chess.Rules
import IO.Standard.Command
import IO.Standard.ProgramState
import System.IO

-- | The prompt.
prompt :: String
prompt = "> "

{- | Loop while asking the user for input commands.

Start with a given starting 'GameState'
-}
loop :: ProgramState -> IO ()
loop ps@ProgramState{game = gs@GameState{board = curBoard, ..}, ..} =
  case (getEndType gs) of
    Just endType -> putStrLn $ endTypeStr endType
    Nothing ->
      do
        progState <- case (bots $ turn) of
          Nothing -> handleHumanPlayer
          Just bt -> handleBotPlayer bt

        putStrLn . show $ progState
        loop progState
 where
  -- Handle the bot playing
  handleBotPlayer bt =
    -- Call the selection algorithm for this bot...
    let (move, newBot) = (selectMove bt gs)
     in case playMove gs move of
          Left err -> error $ "bot error: " ++ (show err)
          Right newGs ->
            let newPs =
                  -- ...Then when it succeeds, call the loop with the updated game state and bot.
                  ps
                    { bots = (\c -> if c == turn then return newBot else bots c),
                      lastMove = Just $ (getSrcCoord curBoard move, getDstCoord curBoard move),
                      game = newGs
                    }
             in return newPs
  -- Handle a human play
  handleHumanPlayer :: IO ProgramState
  handleHumanPlayer =
    do
      -- Start by displaying the prompt
      putStr prompt
      hFlush stdout
      input <- getLine
      -- Then parse a command and loop without changing the program state while we get errors.
      case (parseCommand input) of
        Right command ->
          do
            case executeCommand ps command of
              Left err -> (putStrLn . show $ err) >> return ps
              Right newPs -> newPs
        Left err -> (putStrLn . show $ err) >> return ps

{- | This little function simply returns a string describing a type of end game
 (draw, win, and reasons).
-}
endTypeStr :: EndGameType -> String
endTypeStr endType =
  case endType of
    Win (col, winType) -> (show col) ++ " wins by " ++ winTypeStr winType
    Draw (drawType) -> "draw: " ++ drawTypeStr drawType
 where
  winTypeStr wt = case wt of
    Checkmate -> "checkmate"
    Resign -> "resignation"
  drawTypeStr dt = case dt of
    Stalemate -> "stalemate"
    FiftyMoves -> "fifty moves without pawn move or capture"
    ThreefoldRepetition -> "threefold repetition of the position"
    DeadPosition -> "reached a dead position"
    _ -> "unimplemented"
