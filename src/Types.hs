{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Lens.Micro
import Data.Text.Zipper (getText)
import qualified Brick.Widgets.List as List
import qualified Brick.Widgets.Edit as Edit

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
  , _files :: List Name (String, String)
  , _regexFrom :: Editor String Name
  , _regexTo :: Editor String Name
  }
makeLenses ''AppState

selectionL :: (List.Splittable t, Traversable t, Semigroup (t e)) => SimpleGetter (List.GenericList n t e) (Maybe e)
selectionL = to (fmap snd . List.listSelectedElement)

editorContentL :: SimpleGetter (Edit.Editor String n) String
editorContentL = Edit.editContentsL . to getText . to concat

type Event = ()

data Match = Match
  { _matchIndex :: Int
  , _matchLength :: Int
  , _captureIndex :: Int
  }
  deriving (Show, Eq)
makeLenses ''Match

matchEnds :: Match -> Int
matchEnds m = (m^.matchIndex) + (m^.matchLength)

type CaptureGroup = [Match]

data TextWithMatch = TextWithMatch
  { _content :: String
  , _mayIndex :: Maybe Int
  }
  deriving (Show)
makeLenses ''TextWithMatch
