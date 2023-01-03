{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Events.Replace (handleReplaceModeEvent, setupReplaceMode, runReplaceEvent, quit) where

import Data.Zipper
import Search (replaceOne)
import Types
import Util

import Brick (BrickEvent, EventM, halt)
import Brick.Widgets.List (listSelectedL)
import Data.Maybe (fromMaybe, isJust)
import Data.TextWithMatch (TextWithMatch(TextWithMatch), captureGroup)
import Lens.Micro ((^.), _Just)
import Lens.Micro.Mtl
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Control.Monad.State

newtype ReplaceEvent a = ReplaceEvent (StateT (ReplaceState, Bool) (EventM Name AppState) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadState ReplaceState) ReplaceEvent where
  get = ReplaceEvent $ state (\(s, q) -> (s, (s, q)))
  put s = ReplaceEvent $ state (\(_, q) -> ((), (s, q)))

runReplaceEvent :: ReplaceState -> ReplaceEvent a -> EventM Name AppState (a, Maybe ReplaceState)
runReplaceEvent rState (ReplaceEvent m) = do
  (a, (s, q)) <- runStateT m (rState, False)
  pure (a, if q then Nothing else Just s)

withApp :: EventM Name AppState a -> ReplaceEvent a
withApp e = ReplaceEvent . StateT $ \s -> fmap (,s) e

quit :: ReplaceEvent ()
quit = do
  withApp $ focus .= FromInput
  ReplaceEvent $ state (\(s,_) -> ((), (s, True)))

setupReplaceMode :: EventM Name AppState (Maybe ReplaceState)
setupReplaceMode = do
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
      case rState' of
        Just rs -> do
          (curReplacement, _) <- runReplaceEvent rs getCurReplacementE
          either (handleReplaceError rs) (const $ pure rState') curReplacement
        Nothing -> pure Nothing

handleReplaceError :: ReplaceState -> String -> EventM Name AppState (Maybe ReplaceState)
handleReplaceError rs err = do
  curError .= Just err
  (_, rs') <- runReplaceEvent rs quit
  pure rs'

getCurReplacementE :: ReplaceEvent (Either String TextWithMatch)
getCurReplacementE = do
  twm <- use (curReplaceFile . zipCursor)
  withApp $ do
    toPattern <- BS.pack . Text.unpack <$> use (regexTo . editorContentL)
    pure . fmap (`TextWithMatch` Nothing) $ replaceOne toPattern twm

getCurReplacement :: ReplaceEvent TextWithMatch
getCurReplacement = either error id <$> getCurReplacementE

replaceCurrentMatch :: ReplaceEvent Bool
replaceCurrentMatch = do
  replacementString <- getCurReplacement
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
  cattedReplacements <- concatContents . toSeq <$> use curReplaceFile
  liftIO $ BS.writeFile fname cattedReplacements
  mState <- withApp $ do
    matchedFiles.listSelectedL._Just += 1
    setupReplaceMode
  case mState of
    Just rState -> put rState
    Nothing -> withApp halt
  pure True
