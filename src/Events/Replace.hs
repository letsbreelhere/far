{-# LANGUAGE PatternSynonyms, LambdaCase, DeriveFunctor,
   GeneralizedNewtypeDeriving #-}
module Events.Replace (handleReplaceModeEvent, setupReplaceMode, runReplaceEvent) where

import Data.Zipper
import Search (replaceOne)
import Types
import Util

import Brick (BrickEvent(VtyEvent), EventM)
import Brick.Widgets.List (listSelectedL)
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (Foldable(toList))
import Data.Maybe (fromMaybe)
import Data.TextWithMatch (TextWithMatch(TextWithMatch), content, captureGroup)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Control.Monad.State (StateT(..), runStateT, MonadState, get, put)

newtype ReplaceEvent a = ReplaceEvent (StateT ReplaceState (EventM Name AppState) a)
  deriving (Functor, Applicative, Monad, MonadState ReplaceState, MonadIO)

runReplaceEvent :: ReplaceState -> ReplaceEvent a -> EventM Name AppState a
runReplaceEvent rState (ReplaceEvent m) = do
  (a, rState') <- runStateT m rState
  replaceState .= Just rState'
  pure a

-- Not entirely safe: if this modifies the replace state within the AppState,
-- things can get out of sync in a probably-error-prone way here.
withApp :: EventM Name AppState a -> ReplaceEvent a
withApp e = ReplaceEvent . StateT $ \rState -> do
  a <- e
  pure (a, rState)

pattern PlainKey :: V.Key -> BrickEvent n e
pattern PlainKey c = VtyEvent (V.EvKey c [])

setupReplaceMode :: EventM Name AppState (Maybe ReplaceState)
setupReplaceMode = do
  -- TODO: Handle cases where we shouldn't start up:
  --  * No valid regex
  --  * No matched files for regex
  matchedFiles.listSelectedL %= \case
    Nothing -> Just 0
    Just i -> Just i
  use primaryTextWithMatchesL >>= \case
    Nothing -> pure Nothing
    Just (fname, selectionWithMatches) -> do
      focus .= Preview
      let zipper = fromMaybe (error "Empty textWithMatches during replace mode?") (mkZipper selectionWithMatches)
          rState =
            ReplaceState
              { _curGroupIndex=negate 1
              , _curReplaceFile=zipper
              , _curFilename=fname
              }
      replaceState .= Just rState
      found <- runReplaceEvent rState seekNextMatch
      unless found $ error "No matches when starting replace mode"
      pure (Just rState)

getReplaceState :: ReplaceEvent ReplaceState
getReplaceState = get

getCurReplacement :: TextWithMatch -> ReplaceEvent TextWithMatch
getCurReplacement twm = withApp $ do
  toPattern <- BS.pack . Text.unpack <$> use (regexTo . editorContentL)
  case replaceOne toPattern twm of
    Just newContent -> pure $ TextWithMatch newContent Nothing
    Nothing -> error "Couldn't replace pattern; there should be an error handler here"

replaceCurrentMatch :: ReplaceState -> ReplaceEvent Bool
replaceCurrentMatch rState = do
  replacementString <- getCurReplacement (rState ^. curReplaceFile . zipCursor)
  curReplaceFile . zipCursor .= replacementString
  seekNextMatch

handleReplaceModeEvent :: BrickEvent Name Event -> ReplaceEvent ()
handleReplaceModeEvent (PlainKey (V.KChar 'y')) = do
  foundNext <- replaceCurrentMatch =<< getReplaceState
  unless foundNext (void writeAndSeekNextFile)

handleReplaceModeEvent (PlainKey (V.KChar 'n')) = do
  found <- seekNextMatch
  if found
     then pure ()
     else void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'Y')) = do
  repeatWhile (replaceCurrentMatch =<< getReplaceState)
  void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'N')) = void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'q')) = withApp $ replaceState .= Nothing
handleReplaceModeEvent _ = pure ()

repeatWhile :: Monad m => m Bool -> m ()
repeatWhile mb = do
  b <- mb
  when b (repeatWhile mb)

seekNextMatch :: ReplaceEvent Bool
seekNextMatch = do
  z <- use curReplaceFile
  case z ^. zipCursor . captureGroup of
    Just _ -> pure True
    Nothing ->
      case cursorNext z of
        Just z' -> do
          curReplaceFile .= z'
          case z' ^. zipCursor . captureGroup of
            Just _ -> do
              curGroupIndex += 1
              pure True
            Nothing -> seekNextMatch
        Nothing -> pure False

writeAndSeekNextFile :: ReplaceEvent Bool
writeAndSeekNextFile = do
  fname <- use curFilename
  cattedReplacements <- BS.concat . map (view content) . toList . toSeq <$> use curReplaceFile
  mState <- withApp $ do
    liftIO $ BS.writeFile fname cattedReplacements
    use (matchedFiles.listSelectedL) >>= \case
      Just i -> matchedFiles.listSelectedL .= Just (i+1)
      Nothing -> error "No current file"
    setupReplaceMode
  case mState of
    Just rState -> put rState
    Nothing -> error "Done"
  pure True
