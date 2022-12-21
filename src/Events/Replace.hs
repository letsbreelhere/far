{-# LANGUAGE PatternSynonyms, LambdaCase #-}
module Events.Replace (handleReplaceModeEvent, setupReplaceMode) where

import Data.Zipper (zipCursor, mkZipper, cursorNext)
import Search (replaceOne)
import Types
import Util (editorContentL, primaryTextWithMatchesL)

import Brick (BrickEvent(VtyEvent), EventM)
import Brick.Widgets.List (listSelectedL)
import Control.Monad (unless, void)
import Data.Maybe (isJust, fromMaybe)
import Data.TextWithMatch (TextWithMatch(TextWithMatch), captureGroup)
import Lens.Micro ((^.), _Just)
import Lens.Micro.Mtl ((+=), (.=), (%=), use)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Graphics.Vty as V

pattern PlainKey :: V.Key -> BrickEvent n e
pattern PlainKey c = VtyEvent (V.EvKey c [])

setupReplaceMode :: EventM Name AppState ()
setupReplaceMode = do
  -- TODO: Handle cases where we shouldn't start up:
  --  * No valid regex
  --  * No matched files for regex
  matchedFiles.listSelectedL %= \case
    Nothing -> Just 0
    Just i -> Just i
  use primaryTextWithMatchesL >>= \case
    Nothing -> error "All files done"
    Just selectionWithMatches -> do
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

handleReplaceModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleReplaceModeEvent (PlainKey (V.KChar 'y')) = do
  foundNext <- replaceCurrentMatch =<< getReplaceState
  unless foundNext (void seekNextFile)

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

seekNextFile :: EventM Name AppState Bool
seekNextFile = do
  matchedFiles.listSelectedL._Just += 1
  setupReplaceMode
  pure True
