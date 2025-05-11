{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | This module contains parsing and execution of the 'Standard' mode
commands.
-}
module IO.Standard.Command (parseCommand, executeCommand) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Moves
import Chess.Pieces
import Chess.Record
import Chess.Rules
import Data.Char
import Data.List
import IO.MoveExpression
import IO.Standard.ProgramState
import qualified IO.UCI.Command as UCI
import IO.UCI.Loop
import IO.UCI.State
import qualified System.Console.ANSI as ANSI

-- | All possible commands in standard mode.
data Command
  = -- | A command to show information on the terminal
    Show ShowCommand
  | -- | Move a piece from one position to another.
    MovePieceCommand Coord Coord
  | -- | Play a move, with a rule check.
    Play MoveExpression
  | -- | Load a new `GameState` from a `FENString`
    Load FENString
  | -- Switch to UCI mode (irreversible)
    UCI

-- | All possible show commands
data ShowCommand
  = -- | Display the board in the terminal.
    ShowBoard
  | {- | Shows the squares attacked by the piece at the given
    'Coord' with colors.
    -}
    ShowAttacks Coord
  | -- | Shows the squares a piece can legally move to
    ShowValidMoves Coord

-- | Error types that can occur when parsing.
data ParserError
  = -- | The input coordinates are invalid.
    InvalidCoordinates
  | -- | The input command is invlid.
    InvalidCommand
  | -- | No command was issued.
    NoCommand
  | -- | An argument is invalid
    InvalidArgument
  | -- | An argument is missing
    MissingArgument
  | -- | Extra unneeded characters
    ExtraCharacters
  deriving (Show)

-- | Errors that can occur when executing a command.
data ExecutionError = InvalidSquare | MoveExpressionError DecodeError | GameError | LoadError RecordParseError
  deriving
    ( -- | The given square is invalid.
      Show
    )

-- | Lower a string for easier analysis
prepareString :: String -> String
prepareString = map toLower

{- | Parse a command

Returns a 'ParserError' if an error occurs.
`isPrefixOf` is used in the following functions to allow the user to enter
partial commands
-}
parseCommand :: String -> Either ParserError Command
parseCommand input =
  case words input of
    (str : strs)
      | cmd `isPrefixOf` "show" -> parseShowCommand $ map prepareString strs
      | cmd `isPrefixOf` "move" -> parseMoveCommand $ map prepareString strs
      | cmd `isPrefixOf` "play" -> parsePlayCommand $ map prepareString strs
      -- We don't want to lower the arguments to load because `FENStrings` are case sensitive.
      | cmd `isPrefixOf` "load" -> parseLoadCommand strs
      -- Let's not just take the prefix for this one, cause you don't want to trigger UCI on accident.
      | cmd == "uci" -> Right UCI
      | otherwise -> Left InvalidCommand
     where
      cmd = prepareString str
    [] -> Left NoCommand

-- | Parse a show command
parseShowCommand :: [String] -> Either ParserError Command
parseShowCommand (str : strs)
  -- Dispatch the subcommands to respective functions parting them
  | str `isPrefixOf` "attacks" = parseShowAttacksCommand strs
  | str `isPrefixOf` "board" = parseShowBoardCommand strs
  | str `isPrefixOf` "moves" = parseShowValidMovesCommand strs
  | otherwise = Left InvalidArgument
 where
  -- This little function parses a coordinate in the given string, or
  -- returns an error
  parseCoordArg [] = Left MissingArgument
  parseCoordArg [str] = maybe (Left InvalidCoordinates) Right $ parseCoord str
  parseCoordArg (_ : _) = Left ExtraCharacters

  -- For parsing requiring a coordinate argument, we simply call the helper function
  parseShowAttacksCommand strs = parseCoordArg strs >>= return . Show . ShowAttacks

  parseShowValidMovesCommand strs = parseCoordArg strs >>= return . Show . ShowValidMoves

  parseShowBoardCommand [] = Right $ Show ShowBoard
  parseShowBoardCommand _ = Left ExtraCharacters
parseShowCommand [] = Left MissingArgument

-- | Parse a move command
parseMoveCommand :: [String] -> Either ParserError Command
parseMoveCommand [[c1, r1, c2, r2]] =
  case do
    src <- parseCoord [c1, r1]
    dst <- parseCoord [c2, r2]
    return (src, dst) of
    Nothing -> Left InvalidCoordinates
    Just (src, dst) -> Right $ MovePieceCommand src dst
parseMoveCommand (_ : _) = Left ExtraCharacters
parseMoveCommand [] = Left MissingArgument

-- | Parse a play command
parsePlayCommand :: [String] -> Either ParserError Command
parsePlayCommand [str] = maybe (Left InvalidArgument) (Right . Play) $ parseMoveExpression str
parsePlayCommand (_ : _) = Left ExtraCharacters
parsePlayCommand [] = Left MissingArgument

-- | Parse a load command
parseLoadCommand :: [String] -> Either ParserError Command
parseLoadCommand strs = Right $ Load $ intercalate " " strs

{- | Execute the given 'Command' with the given `ProgramState`.

Returns an 'ExecutionError' if an error occurs.
-}
executeCommand :: ProgramState -> Command -> Either ExecutionError (IO ProgramState)
executeCommand ps@ProgramState{game = gs@GameState{board = b}} command =
  case command of
    -- Switch to UCI mode
    IO.Standard.Command.UCI -> Right $ do
      state <- UCI.executeGUICommand UCI.UCI $ startingUCIState (uciBot ps)
      -- Let's just call another infinite loop
      loopUCI state
      return ps
    -- Simply show the board
    Show (ShowBoard) -> Right $ return ps
    -- For showing stuff, simply use the color function
    Show (ShowAttacks coord) ->
      Right $ return ps{coloredSquares = colorSquaresOfInterest (attackedSquares b coord) coord}
    -- Show moves that are valid from a coordinate
    Show (ShowValidMoves coord) ->
      Right $ return ps{coloredSquares = colorSquaresOfInterest (map (getDstCoord b) (validMovesFromCoord gs coord)) coord}
    -- Move a piece on the board without rule checks
    (MovePieceCommand src dst) ->
      case movePiece b src dst of
        Right newBoard -> Right $ do
          return ps{game = gs{board = newBoard}, lastMove = Just (src, dst)}
        Left _ -> Left $ InvalidSquare
    -- Play a move, with rules check and everything, advancing the game state
    (Play moveExpr) ->
      case decodeMoveExpression gs moveExpr of
        Left err -> Left $ MoveExpressionError err
        Right move -> case playMove gs move of
          Right newGs ->
            let (src, dst) = (getSrcCoord b move, getDstCoord b move)
             in Right $ return ps{game = newGs, lastMove = Just (src, dst)}
          Left _ -> Left GameError
    -- Load a `FENString`, replacing the current `GameState`.
    (Load fen) ->
      case gameStateFromFENString fen of
        Left recordErr -> Left $ LoadError recordErr
        Right newGs -> Right $ return ps{game = newGs, lastMove = Nothing}
 where
  -- Little function to color the given squares according to the color of
  -- the piece on them.
  colorSquaresOfInterest sqrs coord = zip sqrs (map chooseColor sqrs)
   where
    chooseColor coord =
      case b ! coord of
        Empty -> ANSI.Green
        Occ (Piece (color, _)) -> case chosenPieceColor of
          Nothing -> ANSI.White
          Just chosenColor -> if chosenColor == color then ANSI.Blue else ANSI.Red

    chosenPieceColor = case b ! coord of
      Empty -> Nothing
      Occ (Piece (color, _)) -> Just color
