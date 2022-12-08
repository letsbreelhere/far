{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Lens.Micro
import qualified Brick.Widgets.List as List

data Name = FileBrowser | Preview | FromInput | ToInput
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

type Event = ()

data Match = Match
  { _matchIndex :: Int
  , _matchLength :: Int
  , _captureIndex :: Int
  }
makeLenses ''Match

type CaptureGroup = [Match]
