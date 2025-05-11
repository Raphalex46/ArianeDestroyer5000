module IO.UCI.Loop (loopUCI) where

import IO.UCI.State
import IO.UCI.Command
import System.IO

-- | Main loop for working with UCI.
loopUCI :: UCIState -> IO ()
loopUCI st =
  do
  -- Don't forget to flush!
    hFlush stdout
    input <- getLine
    case parseGUICommand input of
      Left err -> (putStrLn $ show err) >> loopUCI st
      Right command -> do
        newState <- executeGUICommand command st
        loopUCI newState
