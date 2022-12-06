module Rendering (drawUI) where

import Types

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as List
import Lens.Micro

drawUI :: AppState -> [Widget Name]
drawUI s = [withRenderCtx uiRoot s]

uiRoot :: RenderCtx (Widget Name)
uiRoot = do
  cp <- commandPane
  fp <- filesPane
  pp <- previewPane
  pure $ padAll 5 $ C.center $ B.border $ (fp <=> cp) <+> pp

commandPane :: RenderCtx (Widget Name)
commandPane = pure $ showCursor Command (Location (0,0)) (str "Command")

filesPane :: RenderCtx (Widget Name)
filesPane = do
  fc <- viewing focus
  fs <- viewing files
  pure $ padTop (Pad 1) $ List.renderList renderFile (fc == FileBrowser) fs
  where renderFile selected (fname, _) = withAttr (attrName $ if selected then "selected" else "default") (str fname)

previewPane :: RenderCtx (Widget Name)
previewPane = do
  selection <- viewing (files . selectionL . _Just . _2)
  pure $ hLimitPercent 85 $ padRight (Pad 1) $ B.border $ padRight Max $ padBottom Max $ showCursor Preview  (Location (0,0)) $ str (massageForOutput selection)
    where massageForOutput [] = " " -- Avoid displaying empty files with less space
          massageForOutput s = concatMap replaceTabs s
          replaceTabs '\t' = "    "
          replaceTabs c = [c]
