module CmdLineOptions (parseCmdLineOptions, CmdLineOptions(..)) where

import Types

import Options.Applicative
import Text.Regex.PCRE (CompOption, compCaseless, compExtended)

data CmdLineOptions = CmdLineOptions
  { initFiles :: [FilePath]
  , initFromRegex :: Maybe String
  , initToRegex :: Maybe String
  , initRegexOptions :: [RegexOption]
  }

parseCmdLineOptions :: IO CmdLineOptions
parseCmdLineOptions =
  let opts = info (parser <**> helper) mempty
   in execParser opts

parser :: Parser CmdLineOptions
parser = CmdLineOptions
  <$> many (strArgument (metavar "FILES"))
  <*> optional (strOption (long "from" <> short 'f' <> metavar "FROM"))
  <*> optional (strOption (long "to" <> short 't' <> metavar "TO"))
  <*> parseRegexOptions


parseRegexOptions :: Parser [RegexOption]
parseRegexOptions = sequenceA
  [ parseRegexOption 'i' "case-insensitive" compCaseless False
  , parseRegexOption 'e' "extended" compExtended False
  ]

parseRegexOption :: Char -> String -> CompOption -> Bool -> Parser RegexOption
parseRegexOption shortName longName opt def = RegexOption shortName opt <$> flag def (not def) (short shortName <> long longName)
