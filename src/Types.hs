{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Types (module Types) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Lens.Micro
import Lens.Micro.Extras (view)

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

viewing :: Lens' AppState a -> RenderCtx a
viewing l = view l <$> getCtx

type Event = ()
