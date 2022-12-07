{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Lens.Micro
import qualified Brick.Widgets.List as List

data Name = FileBrowser | Preview | Input
  deriving (Show, Ord, Eq)

instance Enum Name where
  toEnum i = case i `mod` 3 of
               0 -> FileBrowser
               1 -> Preview
               2 -> Input
               _ -> error "What"
  fromEnum n = case n of
                 FileBrowser -> 0
                 Preview -> 1
                 Input -> 2

data AppState = AppState
  { _focus :: Name
  , _files :: List Name (String, String)
  , _regex :: Editor String Name
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
