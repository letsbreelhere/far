{-# LANGUAGE PatternSynonyms #-}
module Events.Replace (handleReplaceModeEvent, setupReplaceMode) where

import Data.Zipper (zipCursor, mkZipper, cursorNext)
import Search (replaceOne)
import Types
import Util (editorContentL, compiledRegexL, textWithMatchesL)

import Brick (BrickEvent(VtyEvent), EventM)
import Brick.Widgets.List (listSelectedL)
import Control.Monad (when, void)
import Data.Maybe (isJust, fromMaybe)
import Data.TextWithMatch (TextWithMatch(TextWithMatch), captureGroup)
import Lens.Micro ((^.), _Just)
import Lens.Micro.Mtl ((+=), (.=), use)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Graphics.Vty as V

pattern PlainKey :: V.Key -> BrickEvent n e
pattern PlainKey c = VtyEvent (V.EvKey c [])

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

seekNextFile :: EventM Name AppState Bool
seekNextFile = do
  i <- fromMaybe (error "that don't make no sense, sarge") <$> use (matchedFiles.listSelectedL)
  matchedFiles.listSelectedL.= Just (i+1)
  setupReplaceMode (i+1)
  pure True
