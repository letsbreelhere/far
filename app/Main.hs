module Main (main) where

import Types
import Rendering
import Gitignore

import Brick
import System.Directory
import Lens.Micro
import qualified Data.Foldable as L
import qualified Brick.Widgets.List as List
import qualified Brick.Widgets.Edit as Edit
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Data.ByteString as BS
import Control.Monad (filterM)
import Events

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = L.find (hasName (s^.focus))
  where hasName n cl = cursorLocationName cl == Just n

mapForApp :: AttrMap
mapForApp = attrMap V.defAttr
  [ (attrName "input", V.currentAttr `V.withForeColor` V.blue)
  , (attrName "selection", V.currentAttr `V.withForeColor` V.blue)
  , (attrName "highlight", V.currentAttr `V.withStyle` V.reverseVideo)
  , (attrName "highlightSelection", V.currentAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  ]

ui :: App AppState Event Name
ui = App
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const mapForApp
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  }

main :: IO ()
main = do
  fs <- filterM doesFileExist =<< getFilteredContents
  fsWithContents <- mapM (\f -> fmap (f ,) (BS.readFile f)) fs
  let fList = List.list FileBrowser (Vec.fromList fsWithContents) 1
      editorFrom = Edit.editor FromInput (Just 1) ""
      editorTo = Edit.editor ToInput (Just 1) ""
  _ <- defaultMain ui (AppState FromInput fList editorFrom editorTo)
  pure ()
