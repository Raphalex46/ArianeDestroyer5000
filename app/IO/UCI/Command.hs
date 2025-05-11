{-# LANGUAGE RecordWildCards #-}

-- | Module containing parser and executor for UCI commands.
module IO.UCI.Command (parseGUICommand, executeGUICommand, UCIGUICommand (..)) where

import Bot.Bot
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Record
import Chess.Rules
import Data.List
import IO.MoveExpression
import IO.UCI.State

-- | Error type for the UCI parser.
data UCIParseError
  = InvalidCommand

instance Show UCIParseError where
  show InvalidCommand = "invalid command"

-- | A UCI command issued from the GUI.
data UCIGUICommand = UCI | Position GameState [Move] | Go | IsReady

-- | A UCI command issued by the engine.
data UCIEngineCommand = Id IdArg

instance Show UCIEngineCommand where
  show (Id arg) = "id " ++ show arg

-- | Arguments to the ID command
data IdArg = Name String | Author String

instance Show IdArg where
  show (Name str) = "name " ++ str
  show (Author str) = "author " ++ str

-- | Parse a UCI command coming from the GUI.
parseGUICommand :: String -> Either UCIParseError UCIGUICommand
parseGUICommand str =
  let tokens = words str
   in case tokens of
        [] -> Left InvalidCommand
        keyword : args ->
          case keyword of
            "uci" -> Right UCI
            "position" -> parsePosition args
            "go" -> Right Go
            "isready" -> Right IsReady
            _ -> Left InvalidCommand

-- | Parse a position received as a UCI command.
parsePosition :: [String] -> Either UCIParseError UCIGUICommand
parsePosition str =
  case str of
    "startpos" : args -> case args of
      "moves" : moves -> return $ Position getStartingGameState $ map parseMove moves
      _ -> return $ Position getStartingGameState []
    "fen" : args ->
      let fenString = intercalate " " $ takeWhile (/= "moves") args
          moves = reverse . takeWhile (/= "moves") $ reverse args
       in case gameStateFromFENString fenString of
            Left _ -> Left InvalidCommand
            Right gs -> return $ Position gs (map parseMove moves)
    _ -> Left InvalidCommand
 where
  parseMove x =
    case parseMoveExpression x of
      Nothing -> error "error while parsing moves"
      Just me -> case me of
        ConcreteMove move -> move
        _ -> error "error while parsing move"

-- | Execute the action associated with the given UCI GUI command.
executeGUICommand :: UCIGUICommand -> UCIState -> IO UCIState
executeGUICommand UCI st =
  do
    putStrLn $ show $ Id (Name "ArianeDestroyer5000")
    putStrLn $ show $ Id (Author "Raphael Colin")
    putStrLn "uciok"
    return st
executeGUICommand (Position gs moves) st =
  return
    st
      { gameState =
          foldl
            ( \acc x -> case playMove acc x of
                Left _ -> error "error applying move in UCI command"
                Right gs -> gs
            )
            gs
            moves
      }
executeGUICommand Go st@UCIState{..} =
  let (move, newBot) = selectMove bot gameState
      strMove = (showCoord $ getSrcCoord (board gameState) move) ++ (showCoord $ getDstCoord (board gameState) move)
   in (putStrLn $ "bestmove " ++ strMove) >> return st{bot = newBot}
executeGUICommand IsReady st =
  putStrLn "readyok" >> return st
