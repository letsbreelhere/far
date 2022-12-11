module Gitignore (getFilteredContents) where

import Control.Monad (join)
import Data.Sequence (Seq)
import GHC.IO (unsafeInterleaveIO)
import System.Directory
import System.FilePath
import Util
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified System.FilePath.Glob as Glob
import Debug.Trace (traceM)

data Pattern = Pattern
  { pNegated :: Bool
  , globPattern :: Glob.Pattern
  }
  deriving (Show)

getFilteredContents :: FilePath -> IO (Seq FilePath)
getFilteredContents path = do
  patterns <- gitIgnorePatterns path
  getDirFiltered (pure . filterPath patterns) path

gitIgnorePatterns :: FilePath -> IO [Pattern]
gitIgnorePatterns path = do
  exists <- doesFileExist $ path </> ".gitignore"
  fmap (lineToPattern (path </> ".git/") ++) $ if exists
    then concatMap (lineToPattern . (path </>)) . lines <$> readFile (path </> ".gitignore")
    else pure []

filterPath :: Foldable t => t Pattern -> FilePath -> Bool
filterPath ps fname = not . any (matchPattern fname) $ ps

matchPattern :: FilePath -> Pattern -> Bool
matchPattern fname (Pattern negated glob) =
  let match = Glob.match glob fname
   in if negated then not match else match

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO (Seq FilePath)
getDirFiltered predicate path = do
    traceM path
    paths <- Seq.fromList <$> listDirectory path
    filteredPaths <- filterMSeq predicate (mkRel <$> paths)
    dirs <- filterMSeq doesDirectoryExist filteredPaths
    next <- unsafeInterleaveIO . fmap join . mapM (getDirFiltered predicate) $ dirs
    pure $ filteredPaths <> next

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
