{-# LANGUAGE LambdaCase, DeriveFunctor, GeneralizedNewtypeDeriving,
   MultiParamTypeClasses #-}
module Events.Replace (handleReplaceModeEvent, setupReplaceMode, runReplaceEvent) where

import Data.Zipper
import Search (replaceOne)
import Types
import Util

import Brick (BrickEvent, EventM)
import Brick.Widgets.List (listSelectedL)
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (Foldable(toList))
import Data.Maybe (fromMaybe, isJust)
import Data.TextWithMatch (TextWithMatch(TextWithMatch), content, captureGroup)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Control.Monad.State (StateT(..), runStateT, MonadState, state, get, put)

newtype ReplaceEvent a = ReplaceEvent (StateT (ReplaceState, Bool) (EventM Name AppState) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadState ReplaceState) ReplaceEvent where
  get = ReplaceEvent $ state (\(s, b) -> (s, (s, b)))
  put s = ReplaceEvent $ state (\(_, b) -> ((), (s, b)))

runReplaceEvent :: ReplaceState -> ReplaceEvent a -> EventM Name AppState (a, Maybe ReplaceState)
runReplaceEvent rState (ReplaceEvent m) = do
  (a, (s, q)) <- runStateT m (rState, False)
  pure (a, if q then Nothing else Just s)

-- Not entirely safe: if this modifies the replace state within the AppState,
-- things can get out of sync in a probably-error-prone way here.
withApp :: EventM Name AppState a -> ReplaceEvent a
withApp e = ReplaceEvent . StateT $ \(rState, done) -> do
  a <- e
  pure (a, (rState, done))

quit :: ReplaceEvent ()
quit = do
  withApp $ focus .= FromInput
  ReplaceEvent $ state (\(s,_) -> ((), (s, True)))

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
      (found, rState') <- runReplaceEvent rState seekCurOrNextMatch
      unless found $ error "No matches when starting replace mode"
      pure rState'

getCurReplacement :: TextWithMatch -> ReplaceEvent TextWithMatch
getCurReplacement twm = withApp $ do
  toPattern <- BS.pack . Text.unpack <$> use (regexTo . editorContentL)
  case replaceOne toPattern twm of
    Just newContent -> pure $ TextWithMatch newContent Nothing
    Nothing -> error "Couldn't replace pattern; there should be an error handler here"

replaceCurrentMatch :: ReplaceEvent Bool
replaceCurrentMatch = do
  rState <- get
  replacementString <- getCurReplacement (rState ^. curReplaceFile . zipCursor)
  curReplaceFile . zipCursor .= replacementString
  seekNextMatch

handleReplaceModeEvent :: BrickEvent Name Event -> ReplaceEvent ()
handleReplaceModeEvent (PlainKey (V.KChar 'y')) = do
  foundNext <- replaceCurrentMatch
  unless foundNext (void writeAndSeekNextFile)
handleReplaceModeEvent (PlainKey (V.KChar 'Y')) = do
  repeatWhile replaceCurrentMatch
  void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'n')) = do
  found <- seekNextMatch
  if found
     then pure ()
     else void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'N')) = void writeAndSeekNextFile
handleReplaceModeEvent (PlainKey (V.KChar 'q')) = quit
handleReplaceModeEvent _ = pure ()

repeatWhile :: Monad m => m Bool -> m ()
repeatWhile mb = do
  b <- mb
  when b (repeatWhile mb)

onMatch :: Zipper TextWithMatch -> Bool
onMatch z = isJust (z ^. zipCursor . captureGroup)

seekNextMatch :: ReplaceEvent Bool
seekNextMatch = do
  z <- use curReplaceFile
  case cursorNext z of
    Nothing -> pure False
    Just z' -> do
      curReplaceFile .= z'
      seekCurOrNextMatch

seekCurOrNextMatch :: ReplaceEvent Bool
seekCurOrNextMatch = do
  z <- use curReplaceFile
  if onMatch z
     then do
       curGroupIndex += 1
       pure True
     else seekNextMatch

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
