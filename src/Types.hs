{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Brick.Widgets.List as List

data Name = FileBrowser | Preview | Command
  deriving (Show, Ord, Eq)
data AppState = AppState
  { _focus :: Name
  , _files :: List Name (String, String)
  }
makeLenses ''AppState

newtype RenderCtx a = RenderCtx { getRenderCtx :: Reader AppState a }
  deriving (Functor, Applicative, Monad, MonadReader AppState)

withRenderCtx :: RenderCtx a -> AppState -> a
withRenderCtx = runReader . getRenderCtx

getCtx :: RenderCtx AppState
getCtx = RenderCtx ask

viewing :: Getting a AppState a -> RenderCtx a
viewing l = view l <$> getCtx

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
