-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

import Chess.Rules
import Chess.GameState
import IO.Standard.Command
import System.IO

-- | The prompt.
prompt :: String
prompt = "> "

-- | Loop while asking the user for input commands.
--
-- Start with a given starting 'GameState'
loop :: GameState -> IO ()
loop gameState =
  case (getEndType gameState) of
    Just endType -> putStrLn $ endTypeStr endType
    Nothing ->
      do
        putStr prompt
        hFlush stdout
        input <- getLine
        case (parseCommand input) of
          Right command -> do
            case executeCommand gameState command of
              Left err -> putStrLn (show err) >> loop gameState
              Right newBoard -> newBoard >>= loop
          Left err -> putStrLn (show err) >> loop gameState
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
                          _ -> "unimplemented"
