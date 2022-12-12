module Events (handleEvent) where

import Search (mkRegex)
import Types
import Util

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Data.Foldable
import Lens.Micro
import qualified Text.Regex.PCRE as Regex
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Data.Maybe (isJust)
import Control.Monad (when, guard)
import Control.Monad.IO.Class (MonadIO(liftIO))

sendEvent :: Event -> EventM n AppState ()
sendEvent e = do
  chan <- use eventChan
  liftIO $ writeBChan chan e

updateMatchedFiles :: EventM Name AppState ()
updateMatchedFiles = do
  allFiles <- use files
  grepRegex <- use (regexFrom . editorContentL)
  let mRegex = do
        guard (not . Text.null $ grepRegex)
        mkRegex (Text.unpack grepRegex)
      matchedFiles' = case mRegex of
        Just r -> Vec.filter (\(_, c) -> isJust $ Regex.matchOnce r c) allFiles
        Nothing -> allFiles
  sendEvent (MatchedFilesProcessed matchedFiles')

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = focus %= nextName
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = focus %= prevName
handleEvent (AppEvent (FilesProcessed fs)) = do
  files %= flip mappend (Vec.fromList (toList fs))
  updateMatchedFiles
handleEvent (AppEvent (MatchedFilesProcessed fs)) = do
  matchedFiles.listElementsL.= fs
  matchedFiles.listSelectedL.= Just 0

handleEvent e = do
  s <- get
  case s^.focus of
    FileBrowser ->
      case e of
        VtyEvent vtyEvent -> fileBrowserEventHandler vtyEvent
        _ -> pure ()
    FromInput -> do
      prevFrom <- use (regexFrom . editorContentL)
      zoom regexFrom $ Edit.handleEditorEvent e
      newFrom <- use (regexFrom . editorContentL)
      when (prevFrom /= newFrom) updateMatchedFiles
    ToInput -> zoom regexTo $ Edit.handleEditorEvent e
    _ -> pure ()

fileBrowserEventHandler :: V.Event -> EventM Name AppState ()
fileBrowserEventHandler e = zoom matchedFiles $ List.handleListEventVi List.handleListEvent e
