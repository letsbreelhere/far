module Main (main) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import System.Directory

data Name = FileBrowser | Preview
  deriving (Show, Ord, Eq)
type AppState = [String]
type Event = ()

drawUI :: AppState -> [Widget Name]
drawUI s = (:[]) $ C.center $ vLimit 25 $ hLimit 140 $ B.border $ joinBorders $ filesPane s <+> previewPane

filesPane :: AppState -> Widget Name
filesPane s = padTop Max . padRight (Pad 5) $ vBox (map str s) <=> showCursor FileBrowser (Location (0,0)) (str "Command")

previewPane :: Widget n
previewPane = B.border $ padLeft Max $ C.center $ str "Preview"

ui :: App AppState Event Name
ui = (simpleApp emptyWidget) { appDraw = drawUI, appChooseCursor = showFirstCursor }

main :: IO ()
main = do
  fs <- getCurrentDirectory >>= listDirectory
  _ <- defaultMain ui fs
  pure ()
