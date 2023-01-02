module Main (main) where

import CmdLineOptions ( parseCmdLineOptions, CmdLineOptions(CmdLineOptions, initFromRegex, initFiles, initToRegex) )
import Events ( handleEvent, startApp )
import Rendering ( drawUI )
import Types ( AppState(..), Name(FromInput, FileBrowser, ToInput), Event )
import qualified AttrMap

import Brick ( customMain, App(..), CursorLocation(cursorLocationName) )
import Brick.BChan (newBChan)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.Vty (Config(..), Vty, mkVty, defaultConfig)
import System.IO (withFile, IOMode (ReadWriteMode))
import System.Posix.IO (handleToFd, stdInput)
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import System.Posix (queryTerminal)

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = find (hasName (_focus s))
  where hasName n cl = cursorLocationName cl == Just n

appMain :: [FilePath] -> App AppState Event Name
appMain fs = App
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const AttrMap.attrMap
  , appHandleEvent = handleEvent
  , appStartEvent = startApp fs
  }

buildVty :: IO Vty
buildVty = do
  isTty <- queryTerminal stdInput
  if isTty
     then mkVty defaultConfig
     else withFile "/dev/tty" ReadWriteMode $ \h -> do
            fd <- handleToFd h
            mkVty (defaultConfig { inputFd = Just fd, outputFd = Nothing })

main :: IO ()
main = do
  isTty <- queryTerminal stdInput
  CmdLineOptions { initFiles, initToRegex, initFromRegex } <- parseCmdLineOptions
  extraStdinFiles <- do
    if isTty
      then pure []
      else fmap lines getContents
  chan <- newBChan 1000
  let fList = List.list FileBrowser Vec.empty 1
      editorFrom = Edit.editor FromInput (Just 1) (Text.pack $ fromMaybe "" initFromRegex)
      editorTo = Edit.editor ToInput (Just 1) (Text.pack $ fromMaybe "" initToRegex)
      initialState = AppState
        { _focus=FromInput
        , _files=mempty
        , _matchedFiles=fList
        , _replaceState=Nothing
        , _regexFrom=editorFrom
        , _regexTo=editorTo
        , _totalFiles=0
        , _processedFiles=0
        , _eventChan=chan
        , _matchThreadId=Nothing
        }
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) (appMain (initFiles ++ extraStdinFiles)) initialState
  pure ()
