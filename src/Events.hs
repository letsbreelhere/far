module Events (handleEvent) where

import Types

import Util
import Brick
import Lens.Micro
import qualified Brick.Widgets.List as List
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as Edit
import Brick.Widgets.List (listElementsL, listSelectedL)
import Data.Foldable
import qualified Data.Vector as Vec

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = focus %= nextName
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = focus %= prevName
handleEvent (AppEvent (FilesProcessed fs)) = do
  prevSelIx <- use (files . listSelectedL)
  files . listElementsL %= flip mappend (Vec.fromList (toList fs))
  files.listSelectedL .= case prevSelIx of
                             Nothing -> Just 0
                             Just x -> Just x
handleEvent e = do
  s <- get
  case s^.focus of
    FileBrowser ->
      case e of
        VtyEvent vtyEvent -> fileBrowserEventHandler vtyEvent
        _ -> pure ()
    FromInput -> zoom regexFrom $ Edit.handleEditorEvent e
    ToInput -> zoom regexTo $ Edit.handleEditorEvent e
    _ -> pure ()

fileBrowserEventHandler :: V.Event -> EventM Name AppState ()
fileBrowserEventHandler e = zoom files $ List.handleListEventVi List.handleListEvent e
