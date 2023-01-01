module CmdLineOptions (parseCmdLineOptions, CmdLineOptions(..)) where

import Options.Applicative

data CmdLineOptions = CmdLineOptions
  { initFiles :: [FilePath]
  , initToRegex :: Maybe String
  , initFromRegex :: Maybe String
  }
  deriving (Show)

parseCmdLineOptions :: IO CmdLineOptions
parseCmdLineOptions =
  let opts = info (parser <**> helper) mempty
   in execParser opts

parser :: Parser CmdLineOptions
parser = CmdLineOptions
  <$> many (strArgument (metavar "FILES"))
  <*> optional (strOption (long "from" <> short 'f' <> metavar "FROM"))
  <*> optional (strOption (long "to" <> short 't' <> metavar "TO"))
