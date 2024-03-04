{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | This module contains parsing and execution of the 'Standard' mode
-- commands.
module IO.Standard.Command (parseCommand, executeCommand) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.Pieces
import Data.Char
import IO.Board
import qualified System.Console.ANSI as ANSI

-- | All possible commands in standard mode.
data Command
  = -- | Shows the squares attacked by the piece at the given
    -- 'Coord' with colors.
    ShowAttacks Coord
  | -- | Display the board in the terminal.
    ShowBoard
  | -- | Move a piece from one position to another.
    MovePiece Coord Coord

-- | Error types that can occur when parsing.
data ParserError
  = -- | The input coordinates are invalid.
    InvalidCoordinates
  | -- | The input command is invlid.
    InvalidCommand
  | -- | No command was issued.
    NoCommand
  deriving (Show)

-- | Errors that can occur when executing a command.
data ExecutionError = InvalidSquare deriving (-- | The given square is invalid.
                                              Show)

-- | Prepare a string for parsing
prepareString :: String -> [String]
prepareString = words . (map toLower)

-- | Parse a command
--
-- Returns a 'ParserError' if an error occurs.
parseCommand :: String -> Either ParserError Command
parseCommand str =
  -- We split the string and lower it for easier analysis.
  case prepareString str of
    ["show", "attacks", coordStr] ->
      case parseCoord coordStr of
        Nothing -> Left InvalidCoordinates
        Just coord -> Right $ ShowAttacks coord
    ["show", "board"] -> Right ShowBoard
    ["move", [c1, r1, c2, r2]] ->
      case do
        src <- parseCoord [c1, r1]
        dst <- parseCoord [c2, r2]
        return (src, dst) of
        Nothing -> Left InvalidCoordinates
        Just (src, dst) -> Right $ MovePiece src dst
    [] -> Left NoCommand
    _ : _ -> Left InvalidCommand

-- | Execute the given 'Command' with the given 'Board'.
--
-- Returns an 'ExecutionError' if an error occurs.
executeCommand :: Board -> Command -> Either ExecutionError (IO (Board))
executeCommand board ShowBoard = Right $ putStrLn (showBoard board) >> return board
executeCommand board (ShowAttacks coord) =
  Right $ do
    let attackedSqs = attackedSquares board coord
        coloredSquares = zip attackedSqs (map chooseColor attackedSqs)
        coloredBoard = colorSquares (toColoredBoard board) coloredSquares
    putStrLn $ showColoredBoard coloredBoard
    return board
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
executeCommand board (MovePiece src dst) =
  case movePiece board src dst of
    Right newBoard -> Right $ do
      putStrLn $ showBoard newBoard
      return newBoard
    Left _ -> Left $ InvalidSquare
