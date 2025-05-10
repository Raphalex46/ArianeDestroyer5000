module IO.UCI.Command (handleHandshake, handleLoop) where

data UCIError =
  HandshakeError

instance Show UCIError where
  show HandshakeError = "error occured during UCI handshake"

handleHandshake :: String -> Either UCIError (IO ())
handleHandshake "uci" = return $ do
  putStrLn "id name ArianeDestroyer5000id"
  putStrLn "author Raphaël Colin"
  putStrLn "option name OwnBook type check default true"
  putStrLn "uciok"

handleHandshake _ = Left HandshakeError

handleLoop :: String -> Either UCIError (IO ())
handleLoop str =
  let newState
        | ["position", "startpos", "moves"] `isPrefixOf` splittedLine = Loop $ getStartingGameState
        | ["position", "fen"] `isPrefixOf` splittedLine = Loop $ 
