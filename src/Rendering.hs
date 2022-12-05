module Rendering (drawUI) where

import Types

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as List

drawUI :: AppState -> [Widget Name]
drawUI s = [withRenderCtx uiRoot s]

uiRoot :: RenderCtx (Widget Name)
uiRoot = do
  cp <- commandPane
  fp <- filesPane
  pp <- previewPane
  pure $ C.center $ hLimit 140 $ B.border $ (fp <=> cp) <+> pp

commandPane :: RenderCtx (Widget Name)
commandPane = pure $ showCursor Command (Location (0,0)) (str "Command")

filesPane :: RenderCtx (Widget Name)
filesPane = do
  fc <- viewing focus
  fs <- viewing files
  pure $ List.renderList renderFile (fc == FileBrowser) fs
  where renderFile selected (fname, _) = withAttr (attrName $ if selected then "selected" else "default") (str fname)

previewPane :: RenderCtx (Widget Name)
previewPane = pure $ B.border $ C.center $ showCursor Preview  (Location (0,0)) $ str "Preview"
