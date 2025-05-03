{-# LANGUAGE RecordWildCards #-}

-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

import Bot
import Chess.Colors
import Chess.GameState
import Chess.Rules
import IO.GameState
import IO.Standard.Command
import System.IO

-- | A configuration for the game: which players are humans or bots
data Config = Config
  { bots :: (Color -> Maybe Bot)
  }

-- | The prompt.
prompt :: String
prompt = "> "

{- | Loop while asking the user for input commands.

Start with a given starting 'GameState'
-}
loop :: Config -> GameState -> IO ()
loop config@Config{..} gs =
  loop' gs
 where
  loop' gameState =
    case (getEndType gameState) of
      Just endType -> putStrLn $ endTypeStr endType
      Nothing ->
        do
          case (bots $ turn gameState) of
            Nothing -> handleHumanPlayer
            Just bt -> handleBotPlayer bt
   where
    handleBotPlayer bt =
      let (move, newBot) = (selectMove bt gameState)
          newConf = config{bots = (\c -> if c == (turn gameState) then return newBot else bots c)}
       in case playMove gameState move of
            Left err -> error $ "bot error: " ++ (show err)
            Right newBoard -> (putStrLn $ showState newBoard) >> loop newConf newBoard
    handleHumanPlayer =
      do
        putStr prompt
        hFlush stdout
        input <- getLine
        case (parseCommand input) of
          Right command ->
            do
              case executeCommand gameState command of
                Left err -> printErrAndReturn err gameState
                Right newBoard -> newBoard >>= loop'
          Left err -> printErrAndReturn err gameState

    printErrAndReturn err gs = putStrLn (show err) >> (loop' gs)
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
