{-# LANGUAGE TemplateHaskell #-}
module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)

data Name = FileBrowser | Preview | Command
  deriving (Show, Ord, Eq)
data AppState = AppState
  { _focus :: Name
  , _files :: List Name String
  }
makeLenses ''AppState

type Event = ()
