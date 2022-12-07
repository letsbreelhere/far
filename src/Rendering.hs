{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Rendering (drawUI) where

import Types

import Brick
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

newtype RenderCtx a = RenderCtx { getRenderCtx :: Reader AppState a }
  deriving (Functor, Applicative, Monad, MonadReader AppState)

withRenderCtx :: RenderCtx a -> AppState -> a
withRenderCtx = runReader . getRenderCtx

getCtx :: RenderCtx AppState
getCtx = RenderCtx ask

viewing :: Getting a AppState a -> RenderCtx a
viewing l = view l <$> getCtx

drawUI :: AppState -> [Widget Name]
drawUI s = [withRenderCtx uiRoot s]

uiRoot :: RenderCtx (Widget Name)
uiRoot = do
  ip <- inputPane
  fp <- filesPane
  pp <- previewPane
  pure $ padAll 5 $ C.center $ B.border $ (fp <=> ip) <+> pp

inputPane :: RenderCtx (Widget Name)
inputPane = do
  hasFocus <- (Input ==) <$> viewing focus
  editor <- viewing regex
  pure $ E.renderEditor (vBox . map str) hasFocus editor

filesPane :: RenderCtx (Widget Name)
filesPane = do
  fc <- viewing focus
  fs <- viewing files
  pure $ padTop (Pad 1) $ L.renderList renderFile (fc == FileBrowser) fs
  where renderFile selected (fname, _) = withAttr (attrName $ if selected then "selected" else "default") (str fname)

previewPane :: RenderCtx (Widget Name)
previewPane = do
  selection <- viewing (files . selectionL . _Just . _2)
  pure $ hLimitPercent 85 $ padRight (Pad 1) $ B.border $ padRight Max $ padBottom Max $ showCursor Preview  (Location (0,0)) $ str (massageForOutput selection)
    where massageForOutput [] = " " -- Avoid displaying empty files with less space
          massageForOutput s = concatMap replaceTabs s
          replaceTabs '\t' = "    "
          replaceTabs c = [c]
