module Main (main) where

import Types
import Rendering
import Gitignore

import Brick
import System.Directory
import Lens.Micro
import Data.Maybe (catMaybes)
import qualified Data.Foldable as L
import qualified Data.List as L
import qualified Brick.Widgets.List as List
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Data.ByteString as BS
import Data.Text.Encoding
import qualified Data.Text as Text
import Control.Monad (filterM)

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = L.find (hasName (s^.focus))
  where hasName n cl = cursorLocationName cl == Just n

mapForApp :: AttrMap
mapForApp = attrMap V.defAttr
  [ (attrName "default", V.defAttr)
  , (attrName "selected", V.defAttr `V.withStyle` V.reverseVideo)
  ]

eventHandler :: BrickEvent Name Event -> EventM Name AppState ()
eventHandler (VtyEvent e) = do
  s <- get
  case s^.focus of
    FileBrowser -> zoom files $ List.handleListEventVi undefined e
    _ -> pure ()
eventHandler _ = pure ()

ui :: App AppState Event Name
ui = App
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const mapForApp
  , appHandleEvent = eventHandler
  , appStartEvent = pure ()
  }

main :: IO ()
main = do
  fs <- fmap L.sort . filterM doesFileExist =<< getFilteredContents
  fsWithContents <- catMaybes <$> mapM withContents fs
  let fList = List.list FileBrowser (Vec.fromList fsWithContents) 1
  _ <- defaultMain ui (AppState FileBrowser fList)
  pure ()
    where withContents f = do
            contents <- BS.readFile f
            case decodeUtf8' contents of
              Right decoded -> pure $ Just (f, Text.unpack decoded)
              _ -> pure Nothing
