{-# LANGUAGE RecordWildCards #-}
-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

import Chess.Rules
import Chess.Colors
import Chess.GameState
import IO.Standard.Command
import IO.GameState
import System.IO
import Bot

-- | Type of player: can be either a bot or a human
data PlayerType = Human | Bot BotType deriving (Show, Read)

-- | A configuration for the game: which players are humans or bots
data Config = Config {
  player :: (Color -> PlayerType)
}

-- | The prompt.
prompt :: String
prompt = "> "

-- | Loop while asking the user for input commands.
--
-- Start with a given starting 'GameState'
loop :: Config -> GameState -> IO ()
loop Config {..} gs =
  loop' gs
  where
    loop' gameState =
      case (getEndType gameState) of
        Just endType -> putStrLn $ endTypeStr endType
        Nothing ->
          do
            case (bots $ turn gameState) of
              Nothing -> do
                  putStr prompt
                  hFlush stdout
                  input <- getLine
                  case (parseCommand input) of
                    Right command -> do
                      case executeCommand gameState command of
                        Left err -> putStrLn (show err) >> loop' gameState
                        Right newBoard -> newBoard >>= loop'
                    Left err -> putStrLn (show err) >> loop' gameState
              Bot bt ->
                case playMove gameState (selectMove bt gameState) of
                  Left err -> error $ "bot error: " ++ (show err)
                  Right newBoard -> (putStrLn $ showState newBoard) >> loop' newBoard

      where
        endTypeStr endType =
          case endType of
            Win(col, winType) -> (show col) ++ "wins by " ++ winTypeStr winType
            Draw(drawType) -> "draw: " ++ drawTypeStr drawType
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
