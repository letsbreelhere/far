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
import Search (textWithMatches, mkRegex)

newtype RenderCtx a = RenderCtx { getRenderCtx :: Reader AppState a }
  deriving (Functor, Applicative, Monad, MonadReader AppState)

focusIs :: Name -> RenderCtx Bool
focusIs n = (n == ) <$> viewing focus

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
  fromFocus <- focusIs FromInput
  let showEditor = withAttr (attrName "input") . vBox . map str
  f <- E.renderEditor showEditor fromFocus <$> viewing regexFrom

  toFocus <- focusIs ToInput
  t <- E.renderEditor showEditor toFocus <$> viewing regexTo
  pure $ f <+> str "/ " <+> t

filesPane :: RenderCtx (Widget Name)
filesPane = do
  hasFocus <- (FileBrowser ==) <$> viewing focus
  fs <- viewing files
  pure $ padTop (Pad 1) $ L.renderList (renderFile hasFocus) hasFocus fs

renderFile :: Bool -> Bool -> (String, String) -> Widget n
renderFile hasFocus selected (fname, _) = withAttr attr (str fname)
  where attr = attrName $ case (selected, hasFocus) of
                 (True, True) -> "selectedFocus"
                 (True, False) -> "selected"
                 _ -> "default"

previewPane :: RenderCtx (Widget Name)
previewPane = do
  grepRegex <- viewing (regexFrom . editorContentL)
  let mRegex = mkRegex grepRegex
  selection <- case mRegex of
                 Just r -> vBox . concatMap (map (str . massageForOutput) . lines . _content) . textWithMatches r <$> viewing (files . selectionL . _Just . _2)
                 Nothing -> str . massageForOutput <$> viewing (files . selectionL . _Just . _2)
  pure $ hLimitPercent 85 $ padRight (Pad 1) $ B.border $ padRight Max $ padBottom Max $ showCursor Preview  (Location (0,0)) selection
    where massageForOutput [] = " " -- Avoid displaying empty files with less space
          massageForOutput s = concatMap replaceTabs s
          replaceTabs '\t' = "    "
          replaceTabs c = [c]
