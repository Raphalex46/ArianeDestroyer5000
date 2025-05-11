{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Module containing parser and executor for UCI commands.
module IO.UCI.Command (parseGUICommand, executeGUICommand, UCIGUICommand (..)) where

import Bot.Bot
import Chess.GameState
import Chess.Moves
import Chess.Record
import Chess.Rules
import Data.List
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
      "moves" : moves -> let pos = getStartingGameState in return $ Position pos $ parseMoves pos moves
      _ -> return $ Position getStartingGameState []
    "fen" : args ->
      let fenString = intercalate " " $ takeWhile (/= "moves") args
          moves = reverse . takeWhile (/= "moves") $ reverse args
       in case gameStateFromFENString fenString of
            Left _ -> Left InvalidCommand
            Right gs -> return $ Position gs $ parseMoves gs moves
    _ -> Left InvalidCommand
 where
  parseMoves _ [] = []
  parseMoves pos (m : ms) =
    let newMove = fromAlgebraic (board pos) m
     in newMove : (parseMoves (unwrapState $ playMove pos newMove) ms)

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
      strMove = toAlgebraic (board gameState) move
   in (putStrLn $ "bestmove " ++ strMove) >> return st{bot = newBot}
executeGUICommand IsReady st =
  putStrLn "readyok" >> return st
