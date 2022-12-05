{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main (main) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import System.Directory
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified Data.Foldable as L
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as List
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data Name = FileBrowser | Preview | Command
  deriving (Show, Ord, Eq)
data AppState = AppState
  { _focus :: Name
  , _files :: List Name String
  }
makeLenses ''AppState

type Event = ()

commandPane :: Widget Name
commandPane =  showCursor Command (Location (0,0)) (str "Command")

filesPane :: AppState -> Widget Name
filesPane s = padTop Max . padRight (Pad 5) $ fileList
  where fileList = List.renderList renderFile (s^.focus == FileBrowser) (s^.files)
        renderFile selected fname = withAttr (attrName $ if selected then "selected" else "default") (str fname)

previewPane :: Widget Name
previewPane = B.border $ padLeft Max $ C.center $ showCursor Preview  (Location (0,0)) $ str "Preview"

drawUI :: AppState -> [Widget Name]
drawUI s = (:[]) $ C.center $ vLimit 25 $ hLimit 140 $ B.border $ joinBorders $ (filesPane s <=> commandPane) <+> previewPane

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
