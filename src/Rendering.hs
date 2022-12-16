{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}

module Rendering (module Rendering) where

import Search (textWithMatches, mkRegex)
import Types
import Util

import Brick
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)

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
    Just r -> uncurry previewHighlightedContent . textWithMatches r $ selectedContents
    Nothing -> pure . str . massageForWidget . Text.unpack . decodeUtf8 $ selectedContents
  pure $
    selection &
      showCursor Preview (Location (0,0)) &
      padBottom Max &
      padRight Max &
      B.borderWithLabel (str selectedFileName) &
      padRight (Pad 1) &
      hLimitPercent 75

scrollTo :: Location -> Seq (Seq TextWithMatch) -> Seq (Seq TextWithMatch)
scrollTo (Location (x,y)) twms = dropEmptyHead . (_head . content %~ BS.drop x) <$> Seq.drop y twms
  where dropEmptyHead (h:<|t) | BS.null (h^.content) = t
        dropEmptyHead twms' = twms'

previewHighlightedContent :: [CaptureGroup] -> Seq TextWithMatch -> RenderCtx (Widget Name)
previewHighlightedContent _ Seq.Empty = pure $ str " "
previewHighlightedContent cgs twms = do
  curGroupIx <- viewing curGroupIndex
  file <- viewing curFile
  let x = case cgs of
              [] -> 0
              _ -> let curMatch = NE.head . view matches $ cgs !! curGroupIx
                       absoluteIndex = curMatch^.matchStartIndex
                       nearestNewline = fromMaybe (negate 1) . nearestLT absoluteIndex $ mkBinTree (maybe [] (view newlineIndices) file)
                    in absoluteIndex - nearestNewline - 1
      y = 0
      broken = scrollTo (Location (x, y)) $ breakLines twms
      previewAttr twm = case twm^.twmGroupL groupIndex of
                          Just gi ->
                            if gi == curGroupIx then attrName "selectedMatch" else attrName "match"
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
      renderLine [] = Just $ str " "
      renderLine twms' = fmap hBox . mapM renderTwm $ twms'
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
         Nothing -> go (curLine |> s) ss
    firstBreak s = do
      i <- '\n' `BS.elemIndex` s
      pure (BS.take i s, BS.drop (i+1) s)

massageForWidget :: String -> String
massageForWidget [] = " " -- Avoid displaying empty files/lines with less space
massageForWidget s = concatMap replaceTabs s

replaceTabs :: Char -> String
replaceTabs '\t' = "    "
replaceTabs c = [c]
