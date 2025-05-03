{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Chess.Record (gameStateFromFENString, startingFENString, FENString, RecordParseError) where

import Data.Char
import Data.List.Split
import Data.Maybe
import Text.Read

import Chess.Board
import Chess.Colors
import Chess.Coord
import Chess.GameState
import Chess.Moves
import Chess.Pieces

-- | A FENString is just a normal string
type FENString = String

-- | FEN string corresponding to the starting position.
startingFENString :: FENString
startingFENString = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

-- | Error type for parsing errors.
data RecordParseError
  = -- | The input format is invalid.
    InvalidFormat
  | -- | There was an error while parsing a FEN string.
    FENParseError FENParseError
  deriving (Show)

-- | Error type for FEN parsing errors.
data FENParseError
  = InvalidPositions
  | InvalidTurnColor
  | InvalidCastlingRights
  | InvalidCoord
  | InvalidHalfMoveClock
  | InvalidFullMoveClock
  deriving (Show)

{- | Return a game state loaded from a FEN string.

| For more information on FEN strings, see: https://www.chessprogramming.org/Forsyth-Edwards_Notation
-}
gameStateFromFENString :: FENString -> (Either RecordParseError GameState)
gameStateFromFENString str =
  -- A FEN string is made of 6 space-separated fields, we split them up like this:
  case (splitOn " " str) of
    [positions, turnStr, rightsStr, enPassantCoordStr, halfMoveClockStr, fullMoveClockStr] ->
      do
        -- Parse each component with its specific function. See the individual
        -- functions for detail
        board <- boardFromFEN positions
        turn <- turnFromFEN turnStr
        rights <- rightsFromFEN rightsStr
        enPassantCoord <- enPassantCoordFromFEN enPassantCoordStr
        halfMoveClock <- case readMaybe halfMoveClockStr of
          Just clock -> Right clock
          Nothing -> Left $ FENParseError InvalidHalfMoveClock
        fullMoveClock <- case readMaybe fullMoveClockStr of
          Just clock -> Right clock
          Nothing -> Left $ FENParseError InvalidFullMoveClock
        Right
          GameState
            { board = board,
              enPassantCoord = enPassantCoord,
              castlingRights = rights,
              turn = turn,
              halfMoveClock = halfMoveClock,
              fullMoveClock = fullMoveClock,
              history = []
            }
    -- If there are not exactly 6 fields, this is not a FEN string and the
    -- format is invalid
    _ -> Left InvalidFormat

boardFromFEN :: String -> Either RecordParseError Board
boardFromFEN str =
  -- Each row is separated by a '/', we split them up and convert them
  -- to our representation to then construct the board
  let rows = map rowStrToRow rowsStr
      concatRows = concat $ reverse rows
   in if (all isJust concatRows) && (all (== 8) (map length rows))
        then
          Right $ array ((0, 0), (7, 7)) $ zip ([(x, y) | x <- [0 .. 7], y <- [0 .. 7]]) (catMaybes concatRows)
        else
          Left $ FENParseError InvalidPositions
 where
  rowsStr = splitOn "/" str
  -- Local function to convert the FEN representation of a row to our own representation
  rowStrToRow [] = []
  rowStrToRow (h : t)
    | isDigit h = (replicate (read [h]) (Just Empty)) ++ (rowStrToRow t)
    | isAlpha h = case parsePiece h of
        Nothing -> Nothing : rowStrToRow t
        Just p -> (Just $ Occ p) : rowStrToRow t
    | otherwise = Nothing : rowStrToRow t

turnFromFEN :: String -> Either RecordParseError Color
turnFromFEN "w" = Right White
turnFromFEN "b" = Right Black
turnFromFEN _ = Left $ FENParseError InvalidTurnColor

rightsFromFEN :: String -> Either RecordParseError CastlingRights
rightsFromFEN str =
  -- Call to an auxiliary recursive function for building the `CastlingRights`
  -- function. We start by calling it with empty rights.
  rightsFromFEN' str (\_ -> []) >>= (\x -> Right $ CastlingRights x)
 where
  -- Base case: we simply return the rights function
  rightsFromFEN' [] f = Right f
  -- Parse each character from the rights string
  rightsFromFEN' (c : cs) f
    -- Add rights when we encounter the corresponding character
    | c == 'k' = rightsFromFEN' cs (addToF (Black, KingSide))
    | c == 'q' = rightsFromFEN' cs (addToF (Black, QueenSide))
    | c == 'K' = rightsFromFEN' cs (addToF (White, KingSide))
    | c == 'Q' = rightsFromFEN' cs (addToF (White, QueenSide))
    | c == '-' = rightsFromFEN' cs f
    | otherwise = Left $ FENParseError InvalidCastlingRights
   where
    -- Another local function for constructing the castling rights.
    -- Calling this function adds the right to castle to `side` for `col`.
    addToF (col, side) = (\x -> if x == col then side : f x else f x)

enPassantCoordFromFEN :: String -> Either RecordParseError (Maybe Coord)
enPassantCoordFromFEN "-" = Right Nothing
enPassantCoordFromFEN str = case parseCoord str of
  Just coord -> Right $ Just coord
  Nothing -> Left $ FENParseError InvalidCoord
