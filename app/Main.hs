module Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Directory
import Control.Monad.State

type Name = ()
type AppState = [String]
type Event = ()

drawUI :: AppState -> [Widget Name]
drawUI s = (:[]) $ C.center $ vLimit 25 $ hLimit 140 $ B.border $ joinBorders $ filesPane s <+> previewPane

filesPane :: AppState -> Widget n
filesPane s = padTop Max . padBottom (Pad 1) . padRight (Pad 5) $ vBox (map str s) <=> str "Command"

previewPane :: Widget n
previewPane = B.border $ padLeft Max $ C.center $ str "Preview"

ui :: App AppState Event Name
ui = (simpleApp emptyWidget) { appDraw = drawUI }

main :: IO ()
main = do
  fs <- getCurrentDirectory >>= listDirectory
  _ <- defaultMain ui fs
  pure ()
