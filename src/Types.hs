{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes,
   FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable #-}

module Types (module Types) where

import Lens.Micro
import Lens.Micro.Internal
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Sequence (Seq(..), (<|))
import Data.Vector (Vector)
import Brick.BChan (BChan)
import Control.Concurrent (ThreadId)
import qualified Data.Sequence as Seq

data Name = Preview | FileBrowser | FromInput | ToInput
  deriving (Show, Ord, Eq, Enum, Bounded)

data BinTree a = Tip | Branch a (BinTree a) (BinTree a)
  deriving (Show, Functor, Foldable)

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
  , _curGroupIndex :: Maybe Int
  , _regexFrom :: Editor Text Name
  , _regexTo :: Editor Text Name
  , _totalFiles :: Int
  , _eventChan :: BChan Event
  , _matchThreadId :: Maybe ThreadId
  }
makeLenses ''AppState

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

twmGroupL :: Getting a CaptureGroup a -> Getting (Maybe a) TextWithMatch (Maybe a)
twmGroupL getter = captureGroup . to (fmap (view getter))

instance Cons (Seq a) (Seq b) a b where
  _Cons f (a:<|as) = uncurry (<|) <$> f (a, as)
  _Cons _ Seq.Empty = pure Seq.empty
