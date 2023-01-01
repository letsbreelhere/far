{-# LANGUAGE LambdaCase #-}

module Rendering (module Rendering) where

import Data.RenderCtx
import Types
import Widgets.Preview
import qualified AttrMap

import Brick
import Data.ByteString (ByteString)
import Data.Zipper
import Lens.Micro
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Data.Text as Text
import Util (editorContentL)
import Search (replaceOne)

drawUI :: AppState -> [Widget Name]
drawUI s = [withRenderCtx uiRoot s]

uiRoot :: RenderCtx (Widget Name)
uiRoot = do
  inputPane' <- inputPane
  filesPane' <- filesPane
  previewPane' <- previewPane
  progressPane' <- progressPane
  replaceInstructions <- replaceInstructionsPane
  pure $ padAll 5 $ C.center $ B.border $
    (progressPane' <=> filesPane' <=> inputPane') <+> (previewPane' <=> replaceInstructions)

replaceInstructionsPane :: RenderCtx (Widget Name)
replaceInstructionsPane = do
  viewing replaceState >>= \case
     Just rState -> do
       curReplacement <- getCurReplacement rState
       pure . withAttr AttrMap.instructions . str $ "Replace with " ++ fromMaybe "[NOT FOUND]" curReplacement ++ "? y/n/Y/N/q"
     Nothing -> pure emptyWidget

getCurReplacement :: ReplaceState -> RenderCtx (Maybe String)
getCurReplacement rState = do
  let curTwm = rState ^. curReplaceFile . zipCursor
  toPattern <- BS.pack . Text.unpack <$> viewing (regexTo . editorContentL)
  pure $ case replaceOne toPattern curTwm of
    Right newContent -> Just (BS.unpack newContent)
    Left _ -> Nothing

progressPane :: RenderCtx (Widget Name)
progressPane = do
  curFiles <- fromIntegral . length <$> viewing files
  total <- fromIntegral <$> viewing totalFiles
  pure $ if curFiles < total
    then P.progressBar Nothing (curFiles / total)
    else emptyWidget

inputPane :: RenderCtx (Widget Name)
inputPane = do
  fromFocus <- focusIs FromInput
  let showEditor = withAttr AttrMap.input . vBox . map (str . Text.unpack)
  f <- E.renderEditor showEditor fromFocus <$> viewing regexFrom

  toFocus <- focusIs ToInput
  t <- E.renderEditor showEditor toFocus <$> viewing regexTo
  pure $ f <+> str "/ " <+> t

filesPane :: RenderCtx (Widget Name)
filesPane = do
  hasFocus <- (FileBrowser ==) <$> viewing focus
  fs <- viewing matchedFiles
  pure $ L.renderList (renderFile hasFocus) hasFocus fs

renderFile :: Bool -> Bool -> (String, ByteString) -> Widget n
renderFile hasFocus selected (fname, _) = attr (str fname)
  where attr = withAttr $ case (selected, hasFocus) of
                 (True, False) -> AttrMap.selectedFile
                 (True, True) -> AttrMap.focusSelectedFile
                 _ -> mempty
