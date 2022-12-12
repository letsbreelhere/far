{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import Brick.BChan (BChan)
import Control.Concurrent (ThreadId)

data Name = Preview | FileBrowser | FromInput | ToInput
  deriving (Show, Ord, Eq, Enum, Bounded)

data BinTree a = Tip | Branch a (BinTree a) (BinTree a)
  deriving (Show)

mkBinTree :: Ord a => [a] -> BinTree a
mkBinTree [] = Tip
mkBinTree [a] = Branch a Tip Tip
mkBinTree as =
  let midPoint = length as `div` 2
      midElem = as !! midPoint
   in Branch
        midElem
        (mkBinTree (take midPoint as))
        (mkBinTree (drop (midPoint+1) as))

nearest :: (Num a, Ord a) => a -> BinTree a -> Maybe a
nearest = go Nothing
  where
    go mAcc _ Tip = mAcc
    go mAcc a (Branch a' l r) =
      let mAcc' = newBest a a' mAcc
       in case compare a a' of
            EQ -> Just a
            LT -> go mAcc' a l
            GT -> go mAcc' a r
    newBest a b (Just c)
      | abs (a-c) < abs (a-b) = Just c
    newBest _ b _ = Just b


data File = File
  { _fileName :: String
  , _fileContents :: ByteString
  , _newlineIndices :: [Int]
  }
makeLenses ''File

data Event = FilesProcessed (Seq (String, ByteString))
           | MatchedFilesProcessed (Vector (String, ByteString))
           | UpdateThreadId ThreadId

data AppState = AppState
  { _focus :: Name
  , _files :: Vector (String, ByteString)
  , _matchedFiles :: List Name (String, ByteString)
  , _curFile :: File
  , _curMatchIndex :: Int
  , _regexFrom :: Editor Text Name
  , _regexTo :: Editor Text Name
  , _totalFiles :: Int
  , _eventChan :: BChan Event
  , _matchThreadId :: Maybe ThreadId
  }
makeLenses ''AppState

data Match = Match
  { _matchIndex :: Int
  , _matchLength :: Int
  , _captureIndex :: Int
  }
  deriving (Show, Eq)
makeLenses ''Match

type CaptureGroup = [Match]

data TextWithMatch = TextWithMatch
  { _content :: ByteString
  , _mayIndex :: Maybe Int
  }
  deriving (Show)
makeLenses ''TextWithMatch
