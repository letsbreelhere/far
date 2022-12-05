module Main (main) where

import Types
import Rendering

import Brick
import System.Directory
import Lens.Micro
import qualified Data.Foldable as L
import qualified Brick.Widgets.List as List
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

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
    _ -> undefined
eventHandler _ = undefined

ui :: App AppState Event Name
ui = (simpleApp emptyWidget)
  { appDraw = drawUI
  , appChooseCursor = chooseCursor
  , appAttrMap = const mapForApp
  , appHandleEvent = eventHandler
  }

main :: IO ()
main = do
  fs <- getCurrentDirectory >>= listDirectory
  let fList = List.list FileBrowser (Vec.fromList fs) 1
  _ <- defaultMain ui (AppState FileBrowser fList)
  pure ()
