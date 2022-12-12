module Events (handleEvent) where

import Types
import Util

import Brick
import Brick.Widgets.List (listSelectedL, listElementsL)
import Search (findMatches, mkRegex)
import Data.Foldable
import Lens.Micro
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

updateMatchedFiles :: EventM Name AppState ()
updateMatchedFiles = do
  allFiles <- use files
  grepRegex <- use (regexFrom . editorContentL)
  let mRegex = mkRegex $ Text.unpack grepRegex
  let matchedFiles' = case mRegex of
        Just r -> Vec.filter (\(_, c) -> not . null $ findMatches r c) allFiles
        Nothing -> allFiles
  matchedFiles.listElementsL.= matchedFiles'

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = focus %= nextName
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = focus %= prevName
handleEvent (AppEvent (FilesProcessed fs)) = do
  prevSelIx <- use (matchedFiles.listSelectedL)
  files %= flip mappend (Vec.fromList (toList fs))
  updateMatchedFiles

  -- FIXME this should choose the selection index based on the key.
  matchedFiles.listSelectedL.= case prevSelIx of
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
fileBrowserEventHandler e = zoom matchedFiles $ List.handleListEventVi List.handleListEvent e
