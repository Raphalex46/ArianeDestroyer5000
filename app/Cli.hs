module Cli (execOptionsParser, Options (..)) where

import Bot.Bot
import Options.Applicative
import Player

-- | Command line options record.
data Options = Options
  { whitePlayer :: PlayerType,
    blackPlayer :: PlayerType,
    uciBotType :: BotType
  }

-- | Options parser using optparse-applicative.
options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "white"
          <> metavar "TYPE"
          <> help "Type of player for the white pieces"
          <> value Human
      )
    <*> option
      auto
      ( long "black"
          <> metavar "TYPE"
          <> help "Type of player for the black pieces"
          <> value (Bot Random)
      )
    <*> option
      auto
      ( long "uci-bot"
          <> metavar "BOT"
          <> help "Use the specified bot for the engin when running in UCI mode"
          <> value (MinMax)
      )

-- | Options parser with program description.
optionsParser :: ParserInfo Options
optionsParser =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "A chess engine written in Haskell."
        <> header "ArianeDestroyer5000"
    )

-- | Helper function for easier use in the main module.
execOptionsParser :: IO Options
execOptionsParser = execParser optionsParser
