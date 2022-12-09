{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Rendering (module Rendering) where

import Types

import Brick
import Control.Monad.Reader (Reader, MonadReader (ask), runReader)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import Search (textWithMatches, mkRegex)
import Data.List (elemIndex)

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
  ip <- inputPane
  fp <- filesPane
  pp <- previewPane
  pure $ padAll 5 $ C.center $ B.border $ (fp <=> ip) <+> pp

inputPane :: RenderCtx (Widget Name)
inputPane = do
  fromFocus <- focusIs FromInput
  let showEditor = withAttr (attrName "input") . vBox . map str
  f <- E.renderEditor showEditor fromFocus <$> viewing regexFrom

  toFocus <- focusIs ToInput
  t <- E.renderEditor showEditor toFocus <$> viewing regexTo
  pure $ f <+> str "/ " <+> t

filesPane :: RenderCtx (Widget Name)
filesPane = do
  hasFocus <- (FileBrowser ==) <$> viewing focus
  fs <- viewing files
  pure $ padTop (Pad 1) $ L.renderList (renderFile hasFocus) hasFocus fs

renderFile :: Bool -> Bool -> (String, String) -> Widget n
renderFile hasFocus selected (fname, _) = attr (str fname)
  where attr = withAttr $ case (selected, hasFocus) of
                 (True, True) -> attrName "highlightSelection"
                 (True, False) -> attrName "highlight"
                 _ -> mempty

previewPane :: RenderCtx (Widget Name)
previewPane = do
  grepRegex <- viewing (regexFrom . editorContentL)
  let mRegex = mkRegex grepRegex
  selection <- case mRegex of
                 Just r -> previewHighlightedContent . textWithMatches r <$> viewing (files . selectionL . _Just . _2)
                 Nothing -> str . massageForWidget <$> viewing (files . selectionL . _Just . _2)
  pure $ hLimitPercent 85 $ padRight (Pad 1) $ B.border $ padRight Max $ padBottom Max $ showCursor Preview  (Location (0,0)) selection


breakLines :: [TextWithMatch] -> [[TextWithMatch]]
breakLines = map removeLeadingEmpties . go []
  where
    removeLeadingEmpties :: [TextWithMatch] -> [TextWithMatch]
    removeLeadingEmpties (twm:rest@(_:_))
      | null (twm ^. content) = rest
    removeLeadingEmpties twms = twms
    go curLine [] = [curLine]
    go curLine (s:ss) =
       case firstBreak (s^.content) of
         Just (lh, lt) -> (curLine ++ [s & content .~ lh]) : go [] ((s & content .~ lt):ss)
         Nothing -> go (curLine ++ [s]) ss
    firstBreak s = do
      i <-'\n' `elemIndex` s
      pure (take i s, drop (i+1) s)

withAttrs :: [String] -> Widget n -> Widget n
withAttrs = withAttr . foldMap attrName

previewHighlightedContent :: [TextWithMatch] -> Widget Name
previewHighlightedContent twms =
  let broken = breakLines twms
      previewAttr twm = case twm^.mayIndex of
                          Just _ -> ["highlightSelection"]
                          Nothing -> []
      renderLine = hBox . map (\twm -> withAttrs (previewAttr twm) . str . massageForWidget . _content $ twm)
   in vBox $ map renderLine broken

massageForWidget :: String -> String
massageForWidget [] = " " -- Avoid displaying empty files with less space
massageForWidget s = concatMap replaceTabs s

replaceTabs :: Char -> String
replaceTabs '\t' = "    "
replaceTabs c = [c]
