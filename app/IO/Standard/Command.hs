{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains parsing and execution of the 'Standard' mode
-- commands.
module IO.Standard.Command (parseCommand, executeCommand) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.GameState
import Chess.Pieces
import Chess.Rules
import Data.Char
import Data.List
import IO.Board
import IO.MoveExpression
import qualified System.Console.ANSI as ANSI

-- | All possible commands in standard mode.
data Command
  = -- | A command to show information on the terminal
    Show ShowCommand
  | -- | Move a piece from one position to another.
    MovePiece Coord Coord
  |
    Play MoveExpression

-- | All possible show commands
data ShowCommand
  = -- | Display the board in the terminal.
    ShowBoard
  | -- | Shows the squares attacked by the piece at the given
    -- 'Coord' with colors.
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
data ExecutionError = InvalidSquare | MoveExpressionError DecodeError | GameError
  deriving
    ( -- | The given square is invalid.
      Show
    )

-- | Prepare a string for parsing
--
-- We split the string and lower it for easier analysis.
prepareString :: String -> [String]
prepareString = words . (map toLower)

-- | Parse a command
--
-- Returns a 'ParserError' if an error occurs.
-- `isPrefixOf` is used in the following functions to allow the user to enter
-- partial commands
parseCommand :: String -> Either ParserError Command
parseCommand input =
  case prepareString input of
    (str : strs)
      | str `isPrefixOf` "show" -> parseShowCommand strs
      | str `isPrefixOf` "move" -> parseMoveCommand strs
      | str `isPrefixOf` "play" -> parsePlayCommand strs
      | otherwise -> Left InvalidCommand
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
    Just (src, dst) -> Right $ MovePiece src dst
parseMoveCommand (_ : _) = Left ExtraCharacters
parseMoveCommand [] = Left MissingArgument

parsePlayCommand :: [String] -> Either ParserError Command
parsePlayCommand [str] = maybe (Left InvalidArgument) (Right . Play) $ parseMoveExpression str
parsePlayCommand (_:_) = Left ExtraCharacters
parsePlayCommand [] = Left MissingArgument

-- | Execute the given 'Command' with the given 'GameState'.
--
-- Returns an 'ExecutionError' if an error occurs.
executeCommand :: GameState -> Command -> Either ExecutionError (IO GameState)
executeCommand gameState@GameState{..} command =
  case command of
    -- Simply show the board
    Show (ShowBoard) -> Right $ putStrLn (showBoard board) >> return gameState
    -- For showing stuff, simply use the color function
    Show (ShowAttacks coord) ->
      Right $ printColoredSquaresOfInterest (attackedSquares board coord) coord >> return gameState
    Show (ShowValidMoves coord) ->
      Right $ printColoredSquaresOfInterest (validSquaresFromCoord gameState coord) coord >> return gameState
    (MovePiece src dst) ->
      case movePiece board src dst of
        Right newBoard -> Right $ do
          putStrLn $ showBoard newBoard
          return gameState{board=newBoard}
        Left _ -> Left $ InvalidSquare
    (Play moveExpr) ->
          case decodeMoveExpression moveExpr of
            Left err -> Left $ MoveExpressionError err
            Right move -> case playMove gameState move of
              Right newGameState@GameState{board=b} -> Right $ putStrLn (showBoard b) >> return newGameState
              Left _ -> Left GameError
        
  where
    -- Little function to color the given squares according to the color of
    -- the piece on them.
    printColoredSquaresOfInterest sqrs coord =
      do
        let coloredSquares = zip sqrs (map chooseColor sqrs)
            coloredBoard = colorSquares (toColoredBoard board) coloredSquares
        putStrLn $ showColoredBoard coloredBoard
      where
        chooseColor coord =
          case board ! coord of
            Empty -> ANSI.Green
            Occ (Piece (color, _)) -> case chosenPieceColor of
              Nothing -> ANSI.White
              Just chosenColor -> if chosenColor == color then ANSI.Blue else ANSI.Red

        chosenPieceColor = case board ! coord of
          Empty -> Nothing
          Occ (Piece (color, _)) -> Just color
