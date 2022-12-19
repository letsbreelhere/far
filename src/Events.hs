{-# LANGUAGE PatternSynonyms, RankNTypes, FlexibleContexts, LambdaCase #-}
module Events (handleEvent) where

import Data.Zipper
import Search (mkRegex)
import Types
import Util

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Control.Monad (when, guard)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState)
import Data.Foldable
import Data.Maybe (isJust, fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Text.Regex.PCRE as Regex
import Data.TextWithMatch (TextWithMatch(..))

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

enterReplaceMode :: EventM Name AppState ()
enterReplaceMode = do
  grepRegex <- use compiledRegexL
  matchedFiles.listSelectedL .= Just 0
  case grepRegex of
    Nothing -> pure ()
    _ -> do
      selectionWithMatches <- fromMaybe (error "No matches in replace mode?") <$> use textWithMatchesL
      let zipper = fromMaybe (error "Empty textWithMatches during replace mode?") (mkZipper selectionWithMatches)
          rState =
            ReplaceState
              { _curGroupIndex=0
              , _curReplaceFile=zipper
              }
      focus .= Preview
      replaceState .= Just rState

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent e = do
  use modeL >>= \case
    SetupMode -> handleSetupModeEvent e
    ReplaceMode -> handleReplaceModeEvent e

handleSetupModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleSetupModeEvent (PlainKey V.KEsc) = halt
handleSetupModeEvent (PlainKey (V.KChar '\t')) = focus %= nextName
handleSetupModeEvent (PlainKey V.KBackTab) = focus %= prevName
handleSetupModeEvent (PlainKey V.KEnter) = enterReplaceMode
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


getReplaceState :: EventM Name AppState ReplaceState
getReplaceState = fromMaybe (error "No replaceState while handling replace mode event") <$> use replaceState

getCurReplacement :: TextWithMatch -> EventM Name AppState TextWithMatch
getCurReplacement twm = do
  toPattern <- use (regexTo . editorContentL)
  pure $ TextWithMatch { _content=undefined, _captureGroup=Nothing }

handleReplaceModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleReplaceModeEvent (PlainKey (V.KChar 'y')) = do
  rState <- getReplaceState
  replacementString <- getCurReplacement (rState ^. curReplaceFile . zipCursor)
  replaceState . _Just . curReplaceFile . zipCursor .= replacementString
  let z = rState ^. curReplaceFile
  case cursorNext z of
    Just z' -> do
      replaceState . _Just . curReplaceFile .= z'
      replaceState . _Just . curGroupIndex += 1
    Nothing -> undefined

handleReplaceModeEvent (PlainKey (V.KChar 'n')) = pure ()
handleReplaceModeEvent (PlainKey (V.KChar 'q')) = pure ()
handleReplaceModeEvent (PlainKey (V.KChar 'a')) = pure ()
handleReplaceModeEvent _ = pure ()
