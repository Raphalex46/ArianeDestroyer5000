-- | A module for the way the user inputs moves
module IO.MoveExpression (parseMoveExpression, MoveExpression, DecodeError, decodeMoveExpression) where

import Chess.Coord
import Chess.Moves
import Chess.Pieces
import IO.Modes

-- | Move expressions are a data type representing a move that is not
-- necessarily completely defined yet. For example, if the player inputs 'nf6',
-- it means move a knight to f6, but the source coordinate of the knight cannot
-- be determined yet. The move is later on translated to a move standard move.
data MoveExpression
  = -- | A complete, totally determined move.
    ConcreteMove !Move
  | -- | A move expressed with a piece and a
    -- destination position.
    PieceTypeMove !PieceType !Coord

data DecodeError = DecodeError deriving(Show)

-- Just a function to avoid to much repetition.
castle :: Side -> Maybe MoveExpression
castle = Just . ConcreteMove . Castle

-- | Converts a 'String' to a 'MoveExpression'.
--
-- Returns 'Nothing' if the input is invalid.
parseMoveExpression :: String -> Maybe MoveExpression
parseMoveExpression [srcCol, srcRow, dstCol, dstRow] =
  do
    src <- parseCoord [srcCol, srcRow]
    dst <- parseCoord [dstCol, dstRow]
    Just $ ConcreteMove $ MovePiece src dst
parseMoveExpression "O-O" = castle KingSide
parseMoveExpression "o-o" = castle KingSide
parseMoveExpression "0-0" = castle KingSide
parseMoveExpression "O-O-O" = castle QueenSide
parseMoveExpression "o-o-o" = castle QueenSide
parseMoveExpression "0-0-0" = castle QueenSide
parseMoveExpression [p, dstCol, dstRow] =
  do
    ty <- parsePieceType p
    dst <- parseCoord [dstCol, dstRow]
    Just $ PieceTypeMove ty dst
parseMoveExpression dstStr@[_, _] =
  do
    dst <- parseCoord dstStr
    Just $ PieceTypeMove Pawn dst
parseMoveExpression _ = Nothing

-- | Read a move expression from stdin.
readMoveExpression :: Mode -> IO MoveExpression
readMoveExpression Standard =
  do
    line <- getLine
    case parseMoveExpression line of
      Nothing -> do
        putStrLn "Invalid move input. Please enter a different move."
        readMoveExpression Standard
      Just moveExpr -> return moveExpr
readMoveExpression UCI = error "Not yet implemented"

decodeMoveExpression :: MoveExpression -> Either DecodeError Move
decodeMoveExpression (ConcreteMove move) = Right move
decodeMoveExpression (PieceTypeMove _ _) = Left DecodeError
