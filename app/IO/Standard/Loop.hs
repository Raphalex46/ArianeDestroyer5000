-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

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
