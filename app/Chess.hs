{-# LANGUAGE RecordWildCards #-}

module Chess where

import Data.Array
import Data.List
import Data.Char

type Coord = (Int, Int)
type Bounds = (Int, Int)

coord2Letter :: Int -> Char
coord2Letter i = ['A'..'Z'] !! i

letter2Coord :: Char -> Int
letter2Coord char =
  case [i | (c, i) <- zip ['A'..'Z'] [0..], toUpper char == c] of
    [x] -> x
    _ -> error "Unknown character for index"

inBounds :: Coord -> Bounds -> Bool
inBounds (i, j) (upperH, upperW) =
  0 <= i && 0 <= j && i <= upperH && j <= upperW

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King

type Move = (Coord, Coord)

fromString :: String -> Move
fromString [i,j,x,y] = ((letter2Coord i, read [j] - 1), (letter2Coord x, read [y] - 1))
fromString _ = error "Invalid move"

instance Show PieceType where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"

data Color = Black | White deriving(Show, Enum, Eq, Bounded)

cycle :: Color -> Color
cycle c
  | c == maxBound = minBound
  | otherwise = succ c

data Piece = Piece (Color, PieceType)

instance Show Piece where
  show (Piece (Black, p)) = "b" ++ (show p)
  show (Piece (White, p)) = "w" ++ (show p)

data Square = Occ Piece | Empty

instance Show Square where
  show (Occ p) = ' ':show p ++ " "
  show Empty = "    "

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

type Board = Array Coord Square

data Game = Game {
  board :: Board,
  turnNum :: Int,
  turn :: Color,
  lastMove :: Maybe Move,
  canCastle :: Color -> Bool
                 }

startGame :: Game
startGame =
  Game 
  {
    board = startBoard,
    turnNum = 1,
    turn = White,
    lastMove = Nothing,
    canCastle = (\_ -> True)
  }

showGame :: Game -> String
showGame game =
  unlines [
    showBoard $ board game,
    "turn number: " ++ (show $ turnNum game),
    "turn: " ++ (show $ turn game),
    "last move: " ++ (show $ lastMove game),
    "white can castle: " ++ (show $ canCastle game White),
    "black can castle: " ++ (show $ canCastle game Black)
          ]

startBoard :: Board
startBoard =
  let coords =   (\i j -> (i, j)) <$> [0..size - 1] <*> [0..size - 1]
   in array ((0, 0), (size - 1, size - 1)) $ zip coords $ concat $ transpose $ reverse pieces
    where
      size = 8
      emptySquares = replicate size Empty
      base col = [Occ $ Piece (col, Rook), Occ $ Piece (col, Knight), Occ $ Piece (col, Bishop)]
      pawns col = replicate size $ Occ $ Piece (col, Pawn)
      pieces = [
        base Black ++ [Occ $ Piece (Black, Queen), Occ $ Piece (Black, King)] ++ (reverse $ base Black),
        pawns Black,
        emptySquares,
        emptySquares,
        emptySquares,
        emptySquares,
        pawns White,
        base White ++ [Occ $ Piece (White, Queen), Occ $ Piece (White, King)] ++ (reverse $ base White)]

playMove :: Board -> Move -> Board
playMove board (src, dst)
  | not $ inBounds' src  = error "Source coordinate of move is out of bounds!"
  | not $ inBounds' dst  = error "Destination coordinate of move is out of bounds! "
  | otherwise = let newS = board ! src in board // [(src, Empty), (dst, newS)]
  where inBounds' = (flip inBounds) (snd $ bounds board)

showBoard :: Board -> String
showBoard board =
  let boardStr =
        coords ++ "\n" ++ border ++
        (foldl showSquare "" $ map (\(i, j) -> (j, i)) $ indices board)
   in unlines . reverse . lines $ boardStr
    where showSquare acc ((i, j))
            | i == 0 = acc ++ "\n" ++ (show $ j + 1) ++ " |" ++ (show square) ++ "|"
            | i == width = acc ++ (show square) ++ "|\n" ++ border
            | otherwise = acc ++ (show square) ++ "|"
            where square = board ! (i, j)
          width = snd . snd $ bounds board
          squareWidth = length $ show Empty
          border = "  " ++ replicate ((width + 1) * (squareWidth + 1) + 1) '-'
          coords = concat $ "  ":map (\x -> "  " ++ [coord2Letter x] ++ "  ") [0..width]
