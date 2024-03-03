module Chess.Colors (Color (White, Black), opp) where

data Color = White | Black deriving (Eq, Show)

-- | Given a color, returns the opposite color (Black -> White and White -> Black)
opp :: Color -> Color
opp Black = White
opp White = Black
