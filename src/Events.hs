{-# LANGUAGE PatternSynonyms, RankNTypes, FlexibleContexts, LambdaCase #-}
module Events (handleEvent) where

import Events.Replace (handleReplaceModeEvent, setupReplaceMode, runReplaceEvent)
import Search (mkRegex)
import Types
import Util ( nextName, prevName, editorContentL )

import Brick
    ( BrickEvent(VtyEvent, AppEvent), EventM, zoom, get, halt )
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Control.Monad (when, guard, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState)
import Data.Foldable ( Foldable(toList) )
import Data.Maybe (isJust)
import Lens.Micro ( SimpleGetter, (^.) )
import Lens.Micro.Mtl ( (%=), (.=), use )
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Text.Regex.PCRE as Regex

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

pattern PlainKey :: V.Key -> BrickEvent n e
pattern PlainKey c = VtyEvent (V.EvKey c [])

monitorChange :: (MonadState s m, Eq a) => SimpleGetter s a -> (a -> a -> m ()) -> (ev -> m ()) -> ev -> m ()
monitorChange getter onChange handler event = do
  prev <- use getter
  handler event
  next <- use getter
  when (prev /= next) (onChange prev next)

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent e = do
  use replaceState >>= \case
    Nothing -> handleSetupModeEvent e
    Just rState -> runReplaceEvent rState $ handleReplaceModeEvent e

handleSetupModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleSetupModeEvent (PlainKey V.KEsc) = halt
handleSetupModeEvent (PlainKey (V.KChar '\t')) = focus %= nextName
handleSetupModeEvent (PlainKey V.KBackTab) = focus %= prevName
handleSetupModeEvent (PlainKey V.KEnter) = void setupReplaceMode
handleSetupModeEvent (AppEvent (FilesProcessed fs)) = do
  files %= flip mappend (Vec.fromList (toList fs))
  updateMatchedFiles
handleSetupModeEvent (AppEvent (MatchedFilesProcessed fs)) = do
  matchedFiles.listElementsL.= fs
  matchedFiles.listSelectedL.= Just 0
handleSetupModeEvent e = do
  s <- get
  case s^.focus of
    FileBrowser ->
      case e of
        VtyEvent vtyEvent -> zoom matchedFiles $ List.handleListEventVi List.handleListEvent vtyEvent
        _ -> pure ()
    FromInput -> monitorChange (regexFrom . editorContentL) (\_ _ -> updateMatchedFiles) (zoom regexFrom . Edit.handleEditorEvent) e
    ToInput -> zoom regexTo $ Edit.handleEditorEvent e
    _ -> pure ()
