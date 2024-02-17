{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Chess where

import Data.Array
import Data.Char
import Data.List

type Coord = (Int, Int)

type Bounds = (Int, Int)

coord2Letter :: Int -> Char
coord2Letter i = ['A' .. 'Z'] !! i

letter2Coord :: Char -> Int
letter2Coord char =
  case [i | (c, i) <- zip ['A' .. 'Z'] [0 ..], toUpper char == c] of
    [x] -> x
    _ -> error "Unknown character for index"

inBounds :: Coord -> Bounds -> Bool
inBounds (i, j) (upperH, upperW) =
  0 <= i && 0 <= j && i <= upperH && j <= upperW

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq)

pieceFromString :: String -> PieceType
pieceFromString "q" = Queen
pieceFromString "k" = King
pieceFromString "r" = Rook
pieceFromString "b" = Bishop
pieceFromString "n" = Knight
pieceFromString "p" = Pawn
pieceFromString _ = error "Unknown piece"

type Move = (Coord, Coord, Maybe PieceType)

fromString :: String -> Move
fromString str =
  case str of
    [i, j, x, y] -> let (src, dst) = fromString' str in (src, dst, Nothing)
    [i, j, x, y, p] -> let (src, dst) = fromString' [i, j, x, y] in (src, dst, Just $ pieceFromString [p])
    _ -> error "Invalid Move"
  where
    fromString' [i, j, x, y] = ((letter2Coord i, read [j] - 1),
        (letter2Coord x, read [y] - 1))

instance Show PieceType where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"

data Color = Black | White deriving (Show, Enum, Eq, Bounded)

cycleCol :: Color -> Color
cycleCol c
  | c == maxBound = minBound
  | otherwise = succ c

startRank :: Color -> Int
startRank White = 0
startRank Black = 7

pawnStartRank :: Color -> Int
pawnStartRank White = 1
pawnStartRank Black = 6

data Piece = Piece (Color, PieceType)

instance Show Piece where
  show (Piece (Black, p)) = "b" ++ (show p)
  show (Piece (White, p)) = "w" ++ (show p)

data Square = Occ Piece | Empty

instance Show Square where
  show (Occ p) = ' ' : show p ++ " "
  show Empty = "    "

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

isCol :: Square -> Color -> Bool
isCol Empty _ = False
isCol (Occ (Piece (pieceCol, _))) col = pieceCol == col

isType :: Square -> PieceType -> Bool
isType Empty _ = False
isType (Occ (Piece (_, pty))) ty = pty == ty

getType :: Square -> Maybe PieceType
getType Empty = Nothing
getType (Occ (Piece (_, ty))) = Just ty

type Board = Array Coord Square

data Game = Game
  { board :: Board,
    turnNum :: Int,
    turn :: Color,
    lastMove :: Maybe Move,
    canCastle :: Color -> Bool
  }

startGame :: Game
startGame =
  Game
    { board = startBoard,
      turnNum = 1,
      turn = White,
      lastMove = Nothing,
      canCastle = (\_ -> True)
    }

inBetween :: Coord -> Coord -> [Coord]
inBetween (xs, ys) (xd, yd) =
  [(x, y) | (x, y) <- takeWhile (/= (xd, yd)) $ zip [xs + signX, xs + 2 * signX..] [ys + signY, ys + 2 * signY..]]
    where
      signX  = signum $ xd - xs
      signY  = signum $ yd - ys


isEnPassant :: Board -> Move -> Bool
isEnPassant board (src@(xs, _), dst@(xd, _), Nothing) =
  isType (board ! src) Pawn && isEmpty (board ! dst) && xs /= xd
isEnPassant _ (_, _, Just _) = False

isCastle :: Board -> Move -> Bool
isCastle board (src@(xs, ys), dst@(xd, yd), Nothing) =
  isType (board ! src) King && (abs $ xd - xs) == 2
isCastle _ (_, _, Just _) = False

isValid :: Game -> Move -> Bool
isValid game@Game {..} move@(src, dst, prom)
  | isCol srcSquare $ cycleCol turn = False
  | isCol dstSquare turn || isType dstSquare King = False
  | otherwise = case srcSquare of
      Empty -> False
      Occ (Piece (_, ty)) -> isValidMovement game src dst prom ty && (not $ isChecked tempGame turn)
  where
    srcSquare = board ! src
    dstSquare = board ! dst
    tempGame = game {board=playMove board move}

isValidMovement :: Game -> Coord -> Coord -> Maybe PieceType -> PieceType -> Bool
isValidMovement Game {..} (xs, ys) dst@(xd, yd) prom Pawn
  | xs == xd = case sign * (yd - ys) of
      1 -> (isEmpty $ board ! (xd, yd)) && checkProm prom
      2 -> ys == pawnStartRank turn && isEmpty (board ! (xs, ys + sign))
      _ -> False
  | abs (xd - xs) == 1 && yd - ys == sign =
      case board ! dst of
        Occ (Piece (col, _)) -> col == cycleCol turn && checkProm prom
        Empty -> case lastMove of
          Nothing -> False
          Just ((oxs, oys), dstEP@(oxd, oyd), Nothing) ->
            oxs == oxd && oxs == xd && oys == (pawnStartRank $ cycleCol turn) && oyd == oys - sign * 2 && isType (board ! dstEP) Pawn
          Just (_, _, Just _) -> False
  | otherwise = False
  where
    sign = case turn of
      White -> 1
      Black -> -1
    lastRank = case turn of
      White -> 7
      Black -> 0
    checkProm Nothing = yd /= lastRank
    checkProm (Just pieceType) = yd == lastRank && pieceType /= King && pieceType /= Pawn

isValidMovement Game {..} src@(xs, ys) dst@(xd, yd) Nothing Rook =
  (xs == xd || ys == yd) && (all isEmpty $ map (board !) $ inBetween src dst)

isValidMovement Game {..} src@(xs, ys) dst@(xd, yd) Nothing Bishop =
  abs(xd - xs) == abs(yd - ys) && (all isEmpty $ map (board !) $ inBetween src dst)

isValidMovement _ (xs, ys) (xd, yd) Nothing Knight =
  (diffX == 1 && diffY == 2) || (diffX == 2 && diffY == 1)
  where
    diffX = abs $ xd -xs
    diffY = abs $ yd - ys
isValidMovement game src dst Nothing Queen =
  isValidMovement game src dst Nothing Bishop || isValidMovement game src dst Nothing Rook

isValidMovement game@Game{..} src@(xs, ys) dst@(xd, yd) Nothing King
  | moveDistanceX <= 1 && moveDistanceY <= 1 = True
  | moveDistanceX == 2 && moveDistanceY == 0 && ys == startRank turn =
    all id $ map (\x -> checkCastleSquare x (board ! x)) $ dst:inBetween src dst
  | otherwise = False
  where
    moveDistanceX = abs $ xd - xs
    moveDistanceY = abs $ yd - ys
    checkCastleSquare coord square =
      isEmpty square && (not $ isChecked game{board=(board // [(src, Empty), (coord, Occ $ Piece (turn, King))])} turn)

isChecked :: Game -> Color -> Bool
isChecked game@Game{..} col =
  let oppPiecesPos = [pos | (pos, Occ (Piece (c, _))) <- assocs board, c /= col]
  in any attackCheck oppPiecesPos
  where
    kingPos = head [pos | (pos, Occ (Piece (c, King))) <- assocs board, c == col]
    lastRank = case cycleCol turn of
                 White -> 7
                 Black -> 0
    unwrapType sq =
      case getType sq of
        Nothing -> error "Used getype on empty square!"
        Just x -> x
    attackCheck x =
      let ty = unwrapType (board ! x)
          prom = case ty == Pawn && (snd kingPos) == lastRank of
                   True -> Just Queen -- We don't actually care about the type of promotion
                   False -> Nothing
        in isValidMovement game x kingPos prom ty

isCheckMated :: Game -> Color -> Bool
isCheckMated game@Game{..} col
  | isChecked game col =
    let moves = [(src, dst, prom) | src <- indices board, dst <- indices board, prom <- [Nothing, Just Queen]]
    in
    case filter (isValid game) moves of
      [] -> True
      _ -> False
  | otherwise = False
    

showGame :: Game -> String
showGame game =
  let kingChecked = case map (isChecked game) [White, Black] of

                              [True, False] -> Just White
                              [False, True] -> Just Black
                              [False, False] -> Nothing
                              _ -> error "Both kings cannot be in check at once"
  in unlines
    [ showBoard $ board game,
      "turn number: " ++ (show $ turnNum game),
      "turn: " ++ (show $ turn game),
      "last move: " ++ (show $ lastMove game),
      "white can castle: " ++ (show $ canCastle game White),
      "black can castle: " ++ (show $ canCastle game Black),
      "color of king in check: " ++ (show $ kingChecked)
    ]

startBoard :: Board
startBoard =
  let coords = (\i j -> (i, j)) <$> [0 .. size - 1] <*> [0 .. size - 1]
   in array ((0, 0), (size - 1, size - 1)) $ zip coords $ concat $ transpose $ reverse pieces
  where
    size = 8
    emptySquares = replicate size Empty
    base col = [Occ $ Piece (col, Rook), Occ $ Piece (col, Knight), Occ $ Piece (col, Bishop)]
    pawns col = replicate size $ Occ $ Piece (col, Pawn)
    pieces =
      [ base Black ++ [Occ $ Piece (Black, Queen), Occ $ Piece (Black, King)] ++ (reverse $ base Black),
        pawns Black,
        emptySquares,
        emptySquares,
        emptySquares,
        emptySquares,
        pawns White,
        base White ++ [Occ $ Piece (White, Queen), Occ $ Piece (White, King)] ++ (reverse $ base White)
      ]

playMove :: Board -> Move -> Board
playMove board move@(src, dst, prom)
  | not $ inBounds' src = error "Source coordinate of move is out of bounds!"
  | not $ inBounds' dst = error "Destination coordinate of move is out of bounds! "
  | isEnPassant board move = board // [(src, Empty), (dst, newS), ((xd, ys), Empty)]
  | isCastle board move = let signX = signum $ xd - xs
                              row = ys
                              rookPosS = (if signX < 0 then 0 else 7, row)
                              rookPosD = (xd - signX, row)
                             in board // [(src, Empty), (dst, newS), (rookPosS, Empty), (rookPosD, board ! rookPosS)]
  | otherwise = case prom of
                  Nothing -> board // [(src, Empty), (dst, newS)]
                  Just pieceType -> board // [(src, Empty), (dst, Occ $ Piece (newSCol, pieceType))]
  where
    inBounds' = (flip inBounds) (snd $ bounds board)
    newS = board ! src
    ((xs, ys), (xd, yd), prom) = move
    newSCol = case board ! src of
                Empty -> error "shouldn't happen"
                Occ (Piece (col, _)) -> col

showBoard :: Board -> String
showBoard board =
  let boardStr =
        coords
          ++ "\n"
          ++ border
          ++ (foldl showSquare "" $ map (\(i, j) -> (j, i)) $ indices board)
   in unlines . reverse . lines $ boardStr
  where
    showSquare acc ((i, j))
      | i == 0 = acc ++ "\n" ++ (show $ j + 1) ++ " |" ++ (show square) ++ "|"
      | i == width = acc ++ (show square) ++ "|\n" ++ border
      | otherwise = acc ++ (show square) ++ "|"
      where
        square = board ! (i, j)
    width = snd . snd $ bounds board
    squareWidth = length $ show Empty
    border = "  " ++ replicate ((width + 1) * (squareWidth + 1) + 1) '-'
    coords = concat $ "  " : map (\x -> "  " ++ [coord2Letter x] ++ "  ") [0 .. width]
