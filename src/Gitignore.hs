{-# LANGUAGE TemplateHaskell #-}

module Gitignore (getFilteredContents) where

import Lens.Micro
import Lens.Micro.TH
import System.Directory
import System.FilePath
import Control.Monad (filterM)
import GHC.IO (unsafeInterleaveIO)
import qualified System.FilePath.Glob as Glob
import qualified Data.List as L

data Pattern = Pattern
  { _pNegated :: Bool
  , _globPattern :: Glob.Pattern
  }
  deriving (Show)
makeLenses ''Pattern

getFilteredContents :: IO [FilePath]
getFilteredContents = do
  patterns <- gitIgnorePatterns
  getDirFiltered (pure . filterPath patterns) "."

filterPath :: Foldable t => t Pattern -> FilePath -> Bool
filterPath ps fname = not . any (matchPattern fname) $ ps

matchPattern :: FilePath -> Pattern -> Bool
matchPattern fname p =
  let match = Glob.match (p^.globPattern) fname
   in if p^.pNegated then not match else match

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
getDirFiltered predicate path = do
    paths <- L.sort <$> listDirectory path
    filteredPaths <- filterM predicate (mkRel <$> paths)
    dirs <- filterM doesDirectoryExist filteredPaths
    case dirs of
        [] -> pure filteredPaths
        ds -> do
          next <- unsafeInterleaveIO $ foldMapM (getDirFiltered predicate) ds
          pure $ filteredPaths ++ next

    where mkRel = if path == "." then id else (path </>)

foldMapM :: (Applicative m) => (a -> m [b]) -> [a] -> m [b]
foldMapM f = fmap concat . traverse f

lineToPattern :: String -> [Pattern]
lineToPattern "" = []
lineToPattern ('#':_) = []
lineToPattern ('!':p) = map (pNegated .~ True) (lineToPattern p)
lineToPattern patternString = do
  postWildcard <- appendWildcard (stripLeadingDot patternString)
  preWildcard <- prependWildcard postWildcard
  pure $ Pattern False (Glob.compile preWildcard)
  where prependWildcard p
          | "**/" `L.isPrefixOf` p = [p]
          | otherwise = [p, "**/" <> p]
        appendWildcard p
          | "/" `L.isSuffixOf` p = [init p, p <> "**/*"]
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
