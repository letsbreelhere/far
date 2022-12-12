{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Lens.Micro
import Data.Text.Zipper (getText)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Brick.Widgets.List as List
import qualified Brick.Widgets.Edit as Edit
import Data.Sequence (Seq)
import Data.Vector (Vector)

data Name = Preview | FileBrowser | FromInput | ToInput
  deriving (Show, Ord, Eq, Enum, Bounded)

nextName :: Name -> Name
nextName n
  | n < maxBound = toEnum . succ . fromEnum $ n
  | otherwise = minBound

prevName :: Name -> Name
prevName n
  | n > minBound = toEnum . pred . fromEnum $ n
  | otherwise = maxBound

data AppState = AppState
  { _focus :: Name
  , _files :: Vector (String, ByteString)
  , _matchedFiles :: List Name (String, ByteString)
  , _regexFrom :: Editor Text Name
  , _regexTo :: Editor Text Name
  }
makeLenses ''AppState

selectionL ::
  (List.Splittable t, Traversable t, Semigroup (t e)) =>
  SimpleGetter (List.GenericList n t e) (Maybe e)
selectionL = to (fmap snd . List.listSelectedElement)

editorContentL :: SimpleGetter (Edit.Editor Text n) Text
editorContentL = Edit.editContentsL . to getText . to Text.concat

data Event = FilesProcessed (Seq (String, ByteString))

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
