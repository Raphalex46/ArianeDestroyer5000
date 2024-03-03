module Chess.Pieces (PieceType (..), Piece (Piece), parsePiece, showPiece) where

import Chess.Colors
import Data.Char

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Enum)

getTypeChar :: PieceType -> Char
getTypeChar Pawn = 'P'
getTypeChar Rook = 'R'
getTypeChar Knight = 'N'
getTypeChar Bishop = 'B'
getTypeChar Queen = 'Q'
getTypeChar King = 'K'

allTypes :: [PieceType]
allTypes = enumFrom Pawn

data Piece = Piece (Color, PieceType)

parsePieceType :: Char -> Maybe PieceType
parsePieceType char = case [x | x <- allTypes, getTypeChar x == toUpper char] of
  [] -> Nothing
  x : [] -> Just x
  _ : _ -> error "Unreachable"

parsePiece :: Char -> Maybe Piece
parsePiece char =
  do
    ty <- parsePieceType char
    if isUpper char
      then Just $ Piece (White, ty)
      else Just $ Piece (Black, ty)

showPiece :: Piece -> String
showPiece (Piece (col, ty))
  | col == White = [toUpper $ getTypeChar ty]
  | col == Black = [toLower $ getTypeChar ty]
  | otherwise = error "Unreachable"
