module IO.Standard.Command (parseCommand, executeCommand) where

import Chess.Board
import Chess.Coord
import Chess.GameAnalysis
import Chess.Pieces
import Data.Char
import IO.Board
import IO.MoveExpression
import qualified System.Console.ANSI as ANSI

data Command
  = ShowAttacks Coord
  | ShowBoard
  | MovePiece Coord Coord

data ParserError
  = InvalidCoordinates
  | InvalidCommand
  | NoCommand
  deriving (Show)

data ExecutionError = InvalidSquare deriving (Show)

prepareString :: String -> [String]
prepareString = words . (map toLower)

parseCommand :: String -> Either ParserError Command
parseCommand str =
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
executeCommand _ _ = error "Not yet implemented"
