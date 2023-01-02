{-# LANGUAGE RankNTypes, FlexibleContexts, LambdaCase #-}

module Events (handleEvent, startApp) where

import Events.Replace (handleReplaceModeEvent, setupReplaceMode, runReplaceEvent)
import Gitignore
import Search (mkRegex)
import Types
import Util

import Brick
import Brick.BChan (writeBChan, BChan)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Control.Concurrent (forkIO)
import Control.Monad (when, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Data.Foldable ( Foldable(toList) )
import Data.Maybe (isJust, catMaybes)
import Data.Sequence ((><))
import Lens.Micro ( SimpleGetter, (^.) )
import Lens.Micro.Mtl
import System.Directory (doesFileExist)
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Text.Regex.PCRE as Regex
import GHC.IO (unsafeInterleaveIO)
import Data.Text.Encoding (decodeUtf8')

startApp :: [FilePath] -> EventM Name AppState ()
startApp paths = do
  chan <- use eventChan
  let paths' = case paths of
                 [] -> ["."]
                 _ -> paths
  fs <- liftIO $ do
    let concatSeq = foldr (><) Seq.empty
    fss <- mapM getFilteredContents paths'
    fmap Seq.sort . filterMSeq doesFileExist . concatSeq $ fss
  liftIO $ do
    _ <- forkIO $ do
      mapM_ (processFileGroup chan) (Seq.chunksOf 1000 fs)
    pure ()
  totalFiles .= length fs

processFileGroup :: Foldable t => BChan Event -> t FilePath -> IO ()
processFileGroup chan fss = do
  let processSingleFile f = do
        contentsBs <- BS.readFile f
        pure $ case decodeUtf8' contentsBs of
          Left _ -> Nothing
          Right _ -> Just (f, contentsBs)
  readChunks <- fmap (Seq.fromList . catMaybes) . unsafeInterleaveIO . mapM processSingleFile . toList $ fss
  writeBChan chan $ FilesProcessed (length fss) readChunks

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
    Just rState -> do
      (_, rState') <- runReplaceEvent rState $ handleReplaceModeEvent e
      replaceState .= rState'

handleSetupModeEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleSetupModeEvent (PlainKey V.KEsc) = halt
handleSetupModeEvent (PlainKey (V.KChar '\t')) = focus %= nextName
handleSetupModeEvent (PlainKey V.KBackTab) = focus %= prevName
handleSetupModeEvent (PlainKey V.KEnter) = do
  rState <- setupReplaceMode
  replaceState .= rState
handleSetupModeEvent (AppEvent (FilesProcessed processedCount fs)) = do
  files %= flip mappend (Vec.fromList (toList fs))
  processedFiles += processedCount
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
