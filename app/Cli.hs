module Cli (execOptionsParser, Options (..)) where

import Options.Applicative

-- | Type of player : either a bot of a given type or a human.
data PlayerType = Human | Bot BotType deriving(Read, Show)
-- | Type of bot.
data BotType = Random deriving(Read, Show)

-- | Command line options record.
data Options = Options
    { opponent :: PlayerType
    }

-- | Options parser using optparse-applicative.
options :: Parser Options
options =
    Options
        <$> option auto
            ( long "opponent"
                <> metavar "TYPE"
                <> help "Type of opponent to play against."
                <> value Human
            )

-- | Options parser with program description.
optionsParser :: ParserInfo Options
optionsParser =
  info (options <**> helper)
    ( fullDesc
    <> progDesc "A chess engine written in Haskell."
    <> header "ArianeDestroyer5000")

-- | Helper function for easier use in the main module.
execOptionsParser :: IO Options
execOptionsParser = execParser optionsParser
