module Main (main) where

import Types
import Rendering
import Gitignore

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Events
import Lens.Micro
import System.Directory
import System.Environment (getArgs)
import Util
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Brick.Widgets.ProgressBar as Progress
import qualified Data.ByteString as BS
import qualified Data.Foldable as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Graphics.Vty as Vty
import qualified Data.Sequence as Seq

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = L.find (hasName (s^.focus))
  where hasName n cl = cursorLocationName cl == Just n

mapForApp :: AttrMap
mapForApp = attrMap V.defAttr
  [ (Progress.progressCompleteAttr, V.currentAttr `V.withStyle` V.reverseVideo)
  , (attrName "input", V.currentAttr `V.withForeColor` V.blue)
  , (attrName "error", V.currentAttr `V.withForeColor` V.red)
  , (attrName "selectedFile", V.currentAttr `V.withStyle` V.reverseVideo)
  , (attrName "focusSelectedFile", V.currentAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (attrName "match", V.currentAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (attrName "selectedMatch", V.currentAttr `V.withForeColor` V.yellow `V.withStyle` V.reverseVideo)
  ]

ui :: App AppState Event Name
ui = App
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const mapForApp
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  }

buildVty :: IO Vty.Vty
buildVty = Vty.mkVty Vty.defaultConfig

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
               (p:_) -> p
               _ -> "."
  fs <- fmap Seq.sort . filterMSeq doesFileExist =<< getFilteredContents path
  chan <- newBChan 1000
  let process fss = do
        readChunks <- mapM (\f -> fmap (f,) (BS.readFile f)) fss
        writeBChan chan . FilesProcessed $ readChunks
  _ <- forkIO $ do
    mapM_ process (Seq.chunksOf 1000 fs)
  let fList = List.list FileBrowser Vec.empty 1
      editorFrom = Edit.editor FromInput (Just 1) ""
      editorTo = Edit.editor ToInput (Just 1) ""
      initialState = AppState
        { _focus=FromInput
        , _files=mempty
        , _matchedFiles=fList
        , _curMatchIndex=0
        , _curFile=Nothing
        , _regexFrom=editorFrom
        , _regexTo=editorTo
        , _totalFiles=length fs
        , _eventChan=chan
        }
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) ui initialState
  pure ()
