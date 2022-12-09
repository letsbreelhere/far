module Gitignore (getFilteredContents) where

import System.Directory
import System.FilePath
import Control.Monad (filterM)
import GHC.IO (unsafeInterleaveIO)
import qualified System.FilePath.Glob as Glob
import qualified Data.List as L

data Pattern = Pattern
  { pNegated :: Bool
  , globPattern :: Glob.Pattern
  }
  deriving (Show)

getFilteredContents :: IO [FilePath]
getFilteredContents = do
  patterns <- gitIgnorePatterns
  getDirFiltered (pure . filterPath patterns) "."

filterPath :: Foldable t => t Pattern -> FilePath -> Bool
filterPath ps fname = not . any (matchPattern fname) $ ps

matchPattern :: FilePath -> Pattern -> Bool
matchPattern fname (Pattern negated glob) =
  let match = Glob.match glob fname
   in if negated then not match else match

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
getDirFiltered predicate path = do
    paths <- L.sort <$> listDirectory path
    filteredPaths <- filterM predicate (mkRel <$> paths)
    dirs <- filterM doesDirectoryExist filteredPaths
    next <- unsafeInterleaveIO . fmap concat . mapM (getDirFiltered predicate) $ dirs
    pure $ filteredPaths ++ next

    where mkRel = if path == "." then id else (path </>)

lineToPattern :: String -> [Pattern]
lineToPattern "" = []
lineToPattern ('#':_) = []
lineToPattern ('!':p) = map (\p' -> p' { pNegated=True }) (lineToPattern p)
lineToPattern patternString = do
  postWildcard <- appendWildcard (stripLeadingDot patternString)
  preWildcard <- prependWildcard postWildcard
  pure $ Pattern False (Glob.compile preWildcard)
  where prependWildcard p
          | "**/" `L.isPrefixOf` p = [p]
          | otherwise = [p, "**/" <> p]
        appendWildcard p
          | "/" `L.isSuffixOf` p = [p <> "**"]
          | otherwise = [p]
        stripLeadingDot p
          | "./" `L.isPrefixOf` p = drop 2 p
          | otherwise = p

gitIgnorePatterns :: IO [Pattern]
gitIgnorePatterns = do
  exists <- doesFileExist ".gitignore"
  fmap (lineToPattern ".git/" ++) $ if exists
    then concatMap lineToPattern . lines <$> readFile ".gitignore"
    else pure []
