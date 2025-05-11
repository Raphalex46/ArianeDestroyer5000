{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A module for the way the user inputs moves
module IO.MoveExpression (parseMoveExpression, MoveExpression (..), DecodeError, decodeMoveExpression) where

import Chess.Coord
import Chess.GameState
import Chess.Moves
import Chess.Pieces
import IO.Modes

{- | Move expressions are a data type representing a move that is not
necessarily completely defined yet. For example, if the player inputs 'nf6',
it means move a knight to f6, but the source coordinate of the knight cannot
be determined yet. The move is later on translated to a move standard move.
-}
data MoveExpression
  = -- | A complete, totally determined move.
    ConcreteMove !Move
  | {- | A move expressed with a piece and a
    destination position.
    -}
    PieceTypeMove !PieceType !Coord
  | -- | Castling, same as the concrete castle move, except the color is missing
    CastleMove !Side

data DecodeError = DecodeError deriving (Show)

-- Just a function to avoid to much repetition.
castle :: Side -> Maybe MoveExpression
castle = Just . CastleMove

{- | Converts a 'String' to a 'MoveExpression'.

Returns 'Nothing' if the input is invalid.
-}
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
-- Parse pawn promotion
parseMoveExpression [srcCol, srcRow, dstCol, dstRow, pieceType] =
  do
    src <- parseCoord [srcCol, srcRow]
    dst <- parseCoord [dstCol, dstRow]
    pt <- parsePieceType pieceType
    Just $ ConcreteMove $ Promote src dst pt
parseMoveExpression _ = Nothing

-- | Read a move expression from stdin.
_readMoveExpression :: Mode -> IO MoveExpression
_readMoveExpression Standard =
  do
    line <- getLine
    case parseMoveExpression line of
      Nothing -> do
        putStrLn "Invalid move input. Please enter a different move."
        _readMoveExpression Standard
      Just moveExpr -> return moveExpr
_readMoveExpression UCI = error "Not yet implemented"

{- | Given the current `GameState`, decode a `MoveExpression` to an actual
`Move`, usable by the rest of the engine.

The `GameState` is needed to decode moves, for example, when given a
castling move expression, we suppose the castle is for the king of the
current turn.
When typing a `PieceTypeMove`, we need to know where the moved piece is.
-}
decodeMoveExpression :: GameState -> MoveExpression -> Either DecodeError Move
decodeMoveExpression _ (ConcreteMove move) = Right move
decodeMoveExpression _ (PieceTypeMove _ _) = Left DecodeError
decodeMoveExpression GameState{turn = turn} (CastleMove side) = Right $ Castle turn side
