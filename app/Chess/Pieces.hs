-- | Definitions for chess pieces.
module Chess.Pieces
  ( -- | Types.
    PieceType (..),
    Piece (Piece),
    -- | 'Piece' and 'PieceType' operations.
    parsePiece,
    showPiece,
    parsePieceType,
  )
where

import Chess.Colors
import Data.Char

-- | Enum for the types of pieces.
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Enum, Eq, Show)

-- | Returns the character representing the given 'PieceType'.
getTypeChar :: PieceType -> Char
getTypeChar Pawn = 'P'
getTypeChar Rook = 'R'
getTypeChar Knight = 'N'
getTypeChar Bishop = 'B'
getTypeChar Queen = 'Q'
getTypeChar King = 'K'

-- | Returns all piece types.
allTypes :: [PieceType]
allTypes = enumFrom Pawn

-- | A piece is simply a combination of a 'Color' and a 'PieceType'.
data Piece = Piece (Color, PieceType)

-- | Converts a 'Char' to a 'PieceType'.
--
-- Returns 'Nothing' if the input character doesn't represent any piece.
parsePieceType :: Char -> Maybe PieceType
parsePieceType char = case [x | x <- allTypes, getTypeChar x == toUpper char] of
  [] -> Nothing
  x : [] -> Just x
  _ : _ -> error "Unreachable"

-- | Converts a 'Char' to a 'Piece'.
--
-- Calls 'parsePieceType'.
-- If the character is lowercase, it is considered a black piece, if it is
-- uppercase, it is considered a white piece.
parsePiece :: Char -> Maybe Piece
parsePiece char =
  do
    ty <- parsePieceType char
    if isUpper char
      then Just $ Piece (White, ty)
      else Just $ Piece (Black, ty)

-- | Converts a 'Piece' to a 'String'.
showPiece :: Piece -> String
showPiece (Piece (col, ty))
  | col == White = [toUpper $ getTypeChar ty]
  | col == Black = [toLower $ getTypeChar ty]
  | otherwise = error "Unreachable"
