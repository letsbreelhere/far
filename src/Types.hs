{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes,
   FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable #-}

module Types (module Types) where

import Data.Zipper

import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor)
import Brick.Widgets.List (List)
import Control.Concurrent (ThreadId)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Data.Vector (Vector)
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

data Match = Match
  { _matchStartIndex :: Int
  , _matchLength :: Int
  , _captureIndex :: Int
  }
  deriving (Show, Eq)
makeLenses ''Match

data CaptureGroup = CaptureGroup
  { _matches :: NonEmpty Match
  , _groupIndex :: Int
  }
  deriving (Show)
makeLenses ''CaptureGroup

data TextWithMatch = TextWithMatch
  { _content :: ByteString
  , _captureGroup :: Maybe CaptureGroup
  }
  deriving (Show)
makeLenses ''TextWithMatch

data Name = Preview | FileBrowser | FromInput | ToInput
  deriving (Show, Ord, Eq, Enum, Bounded)

data File = File
  { _fileName :: String
  , _fileContents :: ByteString
  , _newlineIndices :: [Int]
  }
makeLenses ''File

data Event = FilesProcessed (Seq (String, ByteString))
           | MatchedFilesProcessed (Vector (String, ByteString))
           | UpdateThreadId ThreadId

data ReplaceState = ReplaceState
  { _curGroupIndex :: Int
  , _curReplaceFile :: Zipper TextWithMatch
  }
makeLenses ''ReplaceState

data AppState = AppState
  { _focus :: Name
  , _files :: Vector (String, ByteString)
  , _matchedFiles :: List Name (String, ByteString)
  , _replaceState :: Maybe ReplaceState
  , _regexFrom :: Editor Text Name
  , _regexTo :: Editor Text Name
  , _totalFiles :: Int
  , _eventChan :: BChan Event
  , _matchThreadId :: Maybe ThreadId
  }
makeLenses ''AppState

twmGroupL :: Getting a CaptureGroup a -> Getting (Maybe a) TextWithMatch (Maybe a)
twmGroupL getter = captureGroup . to (fmap (view getter))
