-- | Small module defining the colors of the teams in chess (black and white).
module Chess.Colors (Color (White, Black), opp, pawnStartingRank) where

-- | Color data type
data Color = White | Black deriving (Eq, Show)

-- | Given a color, returns the opposite color (Black -> White and White -> Black).
opp :: Color -> Color
opp Black = White
opp White = Black

-- | Given a color, return the starting rank of the pawns of that color
pawnStartingRank :: Color -> Int
pawnStartingRank White = 1
pawnStartingRank Black = 6
