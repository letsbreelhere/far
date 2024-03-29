{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Gitignore (getFilteredContents) where

import Control.Monad (join)
import Data.Sequence (Seq)
import GHC.IO (unsafeInterleaveIO, evaluate)
import Lens.Micro
import Lens.Micro.TH
import System.Directory
import System.FilePath
import Util
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified System.FilePath.Glob as Glob
import Control.Monad.Except (throwError)

data Pattern a = Pattern
  { _pNegated :: Bool
  , _globPattern :: a
  }
  deriving (Show, Functor)
makeLenses ''Pattern

getFilteredContents :: FilePath -> IO (Seq FilePath)
getFilteredContents path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       patterns <- gitIgnorePatterns path
       patterns' <- evaluate $ map (fmap Glob.compile) patterns
       getDirFiltered (pure . isPathIgnored patterns') path
     else do
       isFile <- doesFileExist path
       if isFile
          then pure (Seq.singleton path)
          else throwError (userError (path ++ " is not a file or directory"))

gitIgnorePatterns :: FilePath -> IO [Pattern String]
gitIgnorePatterns path = do
  exists <- doesFileExist $ path </> ".gitignore"
  fmap (lineToPattern (path </> ".git/") ++) $ if exists
    then concatMap parseLine . lines <$> readFile (path </> ".gitignore")
    else pure []
  where parseLine line = map (\p -> p & globPattern %~ (path </>)) $ lineToPattern line

-- Returns True if the path should be filtered out. Note that negated patterns
-- are simply passed over. This is a TODO. In reality we should be searching to
-- see if any matching negated files exist and adding them after filtering out.
isPathIgnored :: Foldable t => t (Pattern Glob.Pattern) -> FilePath -> Bool
isPathIgnored ps f = not . any matchIgnorePattern $ ps
  where matchIgnorePattern (Pattern negated glob) = not negated && Glob.match glob f

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO (Seq FilePath)
getDirFiltered predicate path = do
  paths <- Seq.fromList <$> listDirectory path
  filteredPaths <- filterMSeq predicate (mkRel <$> paths)
  dirs <- filterMSeq doesDirectoryExist filteredPaths
  next <- unsafeInterleaveIO . fmap join . mapM (getDirFiltered predicate) $ dirs
  pure $ filteredPaths <> next

  where mkRel = if path == "." then id else (path </>)

lineToPattern :: String -> [Pattern String]
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
        appendWildcard p
          | "/" `L.isSuffixOf` p = [p, p <> "**"]
          | otherwise = [p]
