module Main (main) where

import Types
import Rendering
import Gitignore

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Sequence ((><))
import Events
import Lens.Micro
import Lens.Micro.Mtl
import Options.Applicative
import System.Directory
import System.Environment (getArgs)
import Util
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Brick.Widgets.ProgressBar as Progress
import qualified Data.ByteString as BS
import qualified Data.Foldable as L
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Graphics.Vty as Vty
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

data CmdLineOptions = CmdLineOptions
  { initFiles :: [FilePath]
  , initToRegex :: Maybe String
  , initFromRegex :: Maybe String
  }
  deriving (Show)

parseCmdLineOptions :: Parser CmdLineOptions
parseCmdLineOptions = CmdLineOptions
  <$> many (strArgument (metavar "FILES"))
  <*> optional (strOption (long "from" <> short 'f' <> metavar "FROM"))
  <*> optional (strOption (long "to" <> short 't' <> metavar "TO"))


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
  , (attrName "instructions", V.currentAttr `V.withStyle` V.reverseVideo)
  ]

appMain :: [FilePath] -> App AppState Event Name
appMain fs = App
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const mapForApp
  , appHandleEvent = handleEvent
  , appStartEvent = startApp fs
  }

startApp :: [FilePath] -> EventM Name AppState ()
startApp paths = do
  chan <- use eventChan
  let paths' = case paths of
                 [] -> ["."]
                 _ -> paths
  fs <- liftIO $ do
    fss <- mapM getFilteredContents paths'
    let fs = foldr (><) Seq.empty fss
    fmap Seq.sort . filterMSeq doesFileExist $ fs
  liftIO $ do
    let process fss = do
          readChunks <- mapM (\f -> fmap (f,) (BS.readFile f)) fss
          writeBChan chan . FilesProcessed $ readChunks
    _ <- forkIO $ do
      mapM_ process (Seq.chunksOf 1000 fs)
    pure ()
  totalFiles .= length fs

buildVty :: IO Vty.Vty
buildVty = Vty.mkVty Vty.defaultConfig

main :: IO ()
main = do
  let opts = info (parseCmdLineOptions <**> helper) mempty
  CmdLineOptions initFiles initTo initFrom <- execParser opts
  chan <- newBChan 1000
  let fList = List.list FileBrowser Vec.empty 1
      editorFrom = Edit.editor FromInput (Just 1) (Text.pack $ fromMaybe "" initFrom)
      editorTo = Edit.editor ToInput (Just 1) (Text.pack $ fromMaybe "" initTo)
      initialState = AppState
        { _focus=FromInput
        , _files=mempty
        , _matchedFiles=fList
        , _replaceState=Nothing
        , _regexFrom=editorFrom
        , _regexTo=editorTo
        , _totalFiles=0
        , _eventChan=chan
        , _matchThreadId=Nothing
        }
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) (appMain initFiles) initialState
  pure ()
