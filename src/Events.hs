module Events (handleEvent) where

import Types

import Brick
import Lens.Micro
import qualified Brick.Widgets.List as List
import qualified Graphics.Vty as V

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent e) = do
  s <- get
  case s^.focus of
    FileBrowser -> fileBrowserEventHandler e
    _ -> pure ()
handleEvent e = error $ "Didn't expect " ++ show e

fileBrowserEventHandler :: V.Event -> EventM Name AppState ()
fileBrowserEventHandler e = zoom files $ List.handleListEventVi List.handleListEvent e
