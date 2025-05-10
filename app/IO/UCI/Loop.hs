module IO.UCI.Loop (startUCI) where

import IO.UCI.State
import IO.UCI.Command
import Chess.Record

startUCI :: UCIState -> IO ()
startUCI Handshake =
  do
    command <- getLine
    case handleHandshake command of
      Left err -> putStrLn $ show err
      Right _ -> startUCI $ Loop getStartingGameState
startUCI state@(Loop gs)=
  do
    command <- getLine
    case handleLoop command state of
      Left err -> putStrLn $ show err
      Right io -> io
