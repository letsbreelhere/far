module Events (handleEvent) where

import Types

import Brick
import Lens.Micro
import qualified Brick.Widgets.List as List
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as Edit

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent e = do
  s <- get
  case s^.focus of
    FileBrowser ->
      case e of
        VtyEvent vtyEvent -> fileBrowserEventHandler vtyEvent
        _ -> pure ()
    Input -> zoom regex $ Edit.handleEditorEvent e
    _ -> pure ()

fileBrowserEventHandler :: V.Event -> EventM Name AppState ()
fileBrowserEventHandler e = zoom files $ List.handleListEventVi List.handleListEvent e
