{-# LANGUAGE PatternSynonyms, RankNTypes, FlexibleContexts, LambdaCase #-}
module Events (handleEvent) where

import Data.Zipper
import Search (mkRegex, replaceOne)
import Types
import Util

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Control.Monad (when, guard, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState)
import Data.Foldable
import Data.Maybe (isJust, fromMaybe)
import Data.TextWithMatch
import Lens.Micro
import Lens.Micro.Mtl hiding (view)
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.ByteString.Char8 as BS
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

setupReplaceMode :: Int -> EventM Name AppState ()
setupReplaceMode i = do
  grepRegex <- use compiledRegexL
  matchedFiles.listSelectedL .= Just i
  case grepRegex of
    Nothing -> pure ()
    _ -> do
      replaceState .= Nothing
      selectionWithMatches <- fromMaybe (error "All files done") <$> use textWithMatchesL
      let zipper = fromMaybe (error "Empty textWithMatches during replace mode?") (mkZipper selectionWithMatches)
          rState =
            ReplaceState
              { _curGroupIndex=negate 1
              , _curReplaceFile=zipper
              }
      focus .= Preview
      replaceState .= Just rState
      found <- seekNextMatch
      if found
         then pure ()
         else error $ "No matches when starting replace mode: " ++ show zipper

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent e = do
  use modeL >>= \case
    SetupMode -> handleSetupModeEvent e
    ReplaceMode -> handleReplaceModeEvent e

handleSetupModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleSetupModeEvent (PlainKey V.KEsc) = halt
handleSetupModeEvent (PlainKey (V.KChar '\t')) = focus %= nextName
handleSetupModeEvent (PlainKey V.KBackTab) = focus %= prevName
handleSetupModeEvent (PlainKey V.KEnter) = setupReplaceMode 0
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
  toPattern <- BS.pack . Text.unpack <$> use (regexTo . editorContentL)
  case replaceOne toPattern twm of
    Just newContent -> pure $ TextWithMatch newContent Nothing
    Nothing -> error "Couldn't replace pattern; there should be an error handler here"

replaceCurrentMatch :: ReplaceState -> EventM Name AppState Bool
replaceCurrentMatch rState = do
  replacementString <- getCurReplacement (rState ^. curReplaceFile . zipCursor)
  replaceState . _Just . curReplaceFile . zipCursor .= replacementString
  seekNextMatch

seekNextFile :: EventM Name AppState Bool
seekNextFile = do
  i <- fromMaybe (error "that don't make no sense, sarge") <$> use (matchedFiles.listSelectedL)
  matchedFiles.listSelectedL.= Just (i+1)
  setupReplaceMode (i+1)
  pure True

handleReplaceModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleReplaceModeEvent (PlainKey (V.KChar 'y')) = do
  foundNext <- replaceCurrentMatch =<< getReplaceState
  if foundNext
     then pure ()
     else void seekNextFile

handleReplaceModeEvent (PlainKey (V.KChar 'n')) = do
  found <- seekNextMatch
  if found
     then pure ()
     else void seekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'Y')) = do
  repeatWhile (replaceCurrentMatch =<< getReplaceState)
  void seekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'N')) = void seekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'q')) = replaceState .= Nothing
handleReplaceModeEvent _ = pure ()

repeatWhile :: Monad m => m Bool -> m ()
repeatWhile mb = do
  b <- mb
  when b (repeatWhile mb)

seekNextMatch :: EventM Name AppState Bool
seekNextMatch = do
  rState <- getReplaceState
  let z = rState ^. curReplaceFile
  case cursorNext z of
    Just z' -> do
      replaceState . _Just . curReplaceFile .= z'
      if isJust (z' ^. zipCursor . captureGroup)
         then do
           replaceState . _Just . curGroupIndex += 1
           pure True
         else seekNextMatch
    Nothing -> pure False
