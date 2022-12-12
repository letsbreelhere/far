{-# LANGUAGE TemplateHaskell #-}

module Gitignore (getFilteredContents) where

import Control.Monad (join)
import Data.Sequence (Seq)
import GHC.IO (unsafeInterleaveIO)
import Lens.Micro
import Lens.Micro.TH
import System.Directory
import System.FilePath
import Util
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified System.FilePath.Glob as Glob

data Pattern = Pattern
  { _pNegated :: Bool
  , _globPattern :: String
  }
  deriving (Show)
makeLenses ''Pattern

getFilteredContents :: FilePath -> IO (Seq FilePath)
getFilteredContents path = do
  patterns <- gitIgnorePatterns path
  getDirFiltered (pure . filterPath patterns) path

gitIgnorePatterns :: FilePath -> IO [Pattern]
gitIgnorePatterns path = do
  exists <- doesFileExist $ path </> ".gitignore"
  fmap (lineToPattern (path </> ".git/") ++) $ if exists
    then concatMap parseLine . lines <$> readFile (path </> ".gitignore")
    else pure []
  where parseLine line = map (\p -> p & globPattern %~ (path </>)) $ lineToPattern line

filterPath :: Foldable t => t Pattern -> FilePath -> Bool
filterPath ps fname = not . any (matchPattern fname) $ ps

matchPattern :: FilePath -> Pattern -> Bool
matchPattern fname (Pattern negated glob) =
  let match = Glob.match (Glob.compile glob) fname
   in if negated then not match else match

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO (Seq FilePath)
getDirFiltered predicate path = do
    paths <- Seq.fromList <$> listDirectory path
    filteredPaths <- filterMSeq predicate (mkRel <$> paths)
    dirs <- filterMSeq doesDirectoryExist filteredPaths
    next <- unsafeInterleaveIO . fmap join . mapM (getDirFiltered predicate) $ dirs
    pure $ filteredPaths <> next

    where mkRel = if path == "." then id else (path </>)

lineToPattern :: String -> [Pattern]
lineToPattern "" = []
lineToPattern ('#':_) = []
lineToPattern ('!':p) = map (\p' -> p' & pNegated .~ True) (lineToPattern p)
lineToPattern ('/':p) = lineToPattern p
lineToPattern ('.':'/':p) = lineToPattern p
lineToPattern patternString = do
  postWildcard <- appendWildcard patternString
  preWildcard <- prependWildcard postWildcard
  pure $ Pattern False preWildcard
  where prependWildcard p
          | "**/" `L.isPrefixOf` p = [p]
          | otherwise = [p, "**/" <> p]
        appendWildcard p = [p, p </> "**"]
