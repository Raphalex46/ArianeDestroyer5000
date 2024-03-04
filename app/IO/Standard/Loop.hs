-- | Command loop for the 'Standard' mode.
module IO.Standard.Loop where

import Chess.Board
import IO.Standard.Command
import System.IO

-- | The prompt.
prompt :: String
prompt = "> "

-- | Loop while asking the user for input commands.
--
-- Start with a given starting 'Board'
loop :: Board -> IO ()
loop board =
  do
    putStr prompt
    hFlush stdout
    input <- getLine
    case (parseCommand input) of
      Right command -> do
        case executeCommand board command of
          Left err -> putStrLn (show err) >> loop board
          Right newBoard -> newBoard >>= loop
      Left err -> putStrLn (show err) >> loop board
