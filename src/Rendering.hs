{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Rendering (module Rendering) where

import Search (textWithMatches, mkRegex)
import Types
import Util

import Brick
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Data.Sequence (Seq(..), (<|), (|>))
import Lens.Micro
import Lens.Micro.Extras (view)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import Data.Foldable (Foldable(toList))
import qualified Data.Sequence as Seq
import Data.Text.Encoding (decodeUtf8, decodeUtf8')

newtype RenderCtx a = RenderCtx { getRenderCtx :: Reader AppState a }
  deriving (Functor, Applicative, Monad, MonadReader AppState)

focusIs :: Name -> RenderCtx Bool
focusIs n = (n == ) <$> viewing focus

withRenderCtx :: RenderCtx a -> AppState -> a
withRenderCtx = runReader . getRenderCtx

getCtx :: RenderCtx AppState
getCtx = RenderCtx ask

viewing :: Getting a AppState a -> RenderCtx a
viewing l = view l <$> getCtx

drawUI :: AppState -> [Widget Name]
drawUI s = [withRenderCtx uiRoot s]

uiRoot :: RenderCtx (Widget Name)
uiRoot = do
  inputPane' <- inputPane
  filesPane' <- filesPane
  previewPane' <- previewPane
  progressPane' <- progressPane
  pure $ padAll 5 $ C.center $ B.border $
    (progressPane' <=> filesPane' <=> inputPane') <+> previewPane'

progressPane :: RenderCtx (Widget Name)
progressPane = do
  curFiles <- fromIntegral . length <$> viewing files
  total <- fromIntegral <$> viewing totalFiles
  pure $ if curFiles < total
    then P.progressBar Nothing (curFiles / total)
    else str " "

inputPane :: RenderCtx (Widget Name)
inputPane = do
  fromFocus <- focusIs FromInput
  let showEditor = withAttr (attrName "input") . vBox . map (str . Text.unpack)
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
                 (True, False) -> attrName "selectedFile"
                 (True, True) -> attrName "focusSelectedFile"
                 _ -> mempty

previewPane :: RenderCtx (Widget Name)
previewPane = do
  grepRegex <- viewing (regexFrom . editorContentL)
  let mRegex = mkRegex $ Text.unpack grepRegex
  (selectedFileName, selectedContents) <- viewing (matchedFiles . selectionL . _Just)
  selection <- case mRegex of
    Just r -> previewHighlightedContent . textWithMatches r $ selectedContents
    Nothing -> pure . str . massageForWidget . Text.unpack . decodeUtf8 $ selectedContents
  pure $
    selection &
      showCursor Preview (Location (0,0)) &
      padBottom Max &
      padRight Max &
      B.borderWithLabel (str selectedFileName) &
      padRight (Pad 1) &
      hLimitPercent 75

previewHighlightedContent :: Seq TextWithMatch -> RenderCtx (Widget Name)
previewHighlightedContent Seq.Empty = pure $ str " "
previewHighlightedContent twms = do
  curMatch <- viewing curMatchIndex
  let broken = breakLines twms
      previewAttr twm = case (twm^.mayCaptureIndex, twm^.mayMatchIndex) of
                          (Just _, Just mi) ->
                            if mi == curMatch then attrName "selectedMatch" else attrName "match"
                          _ -> mempty
      attemptDecode :: TextWithMatch -> Maybe String
      attemptDecode twm = case decodeUtf8' . view content $ twm of
                            Left _ -> Nothing
                            Right c -> Just (Text.unpack c)
      renderTwm :: TextWithMatch -> Maybe (Widget Name)
      renderTwm twm = do
        decoded <- attemptDecode twm
        pure . withAttr (previewAttr twm) . str . massageForWidget $ decoded
      renderLine :: [TextWithMatch] -> Maybe (Widget Name)
      renderLine = fmap hBox . mapM renderTwm
  pure $ case mapM (renderLine . toList) (toList broken) of
           Just ls -> vBox ls
           Nothing -> withAttr (attrName "error") $ str "[This file contains invalid characters and will not be displayed.]"


breakLines :: Seq TextWithMatch -> Seq (Seq TextWithMatch)
breakLines = fmap removeLeadingEmpties . go Seq.empty
  where
    removeLeadingEmpties (twm :<| rest@(_:<|_))
      | BS.null (twm ^. content) = rest
    removeLeadingEmpties twms = twms
    go :: Seq TextWithMatch -> Seq TextWithMatch -> Seq (Seq TextWithMatch)
    go curLine Seq.Empty = Seq.singleton curLine
    go curLine (s:<|ss) =
       case firstBreak (s^.content) of
         Just (lh, lt) -> (curLine |> (s & content .~ lh)) <| go Seq.empty ((s & content .~ lt) <| ss)
         Nothing -> go (curLine :|> s) ss
    firstBreak s = do
      i <- '\n' `BS.elemIndex` s
      pure (BS.take i s, BS.drop (i+1) s)

massageForWidget :: String -> String
massageForWidget [] = " " -- Avoid displaying empty files/lines with less space
massageForWidget s = concatMap replaceTabs s

replaceTabs :: Char -> String
replaceTabs '\t' = "    "
replaceTabs c = [c]
