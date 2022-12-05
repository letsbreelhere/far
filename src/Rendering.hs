module Rendering (drawUI) where

import Types

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Lens.Micro
import qualified Brick.Widgets.List as List

commandPane :: Widget Name
commandPane =  showCursor Command (Location (0,0)) (str "Command")

filesPane :: AppState -> Widget Name
filesPane s = List.renderList renderFile (s^.focus == FileBrowser) (s^.files)
  where renderFile selected (fname, _) = withAttr (attrName $ if selected then "selected" else "default") (str fname)

previewPane :: Widget Name
previewPane = B.border $ C.center $ showCursor Preview  (Location (0,0)) $ str "Preview"

drawUI :: AppState -> [Widget Name]
drawUI s = (:[]) $ C.center $ hLimit 140 $ B.border $ (filesPane s <=> commandPane) <+> previewPane
