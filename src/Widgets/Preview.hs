module Widgets.Preview (previewPane) where

import Data.RenderCtx
import Data.TextWithMatch
import Types
import Util

import Brick
import Data.Char
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro
import Lens.Micro.Extras (view, preview)
import qualified Brick.Widgets.Border as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

previewPane :: RenderCtx (Widget Name)
previewPane = do
  (selectedFileName, selectedContents) <- viewing (matchedFiles . selectionL . _Just)
  selection <- viewing textWithMatchesL >>= \case
    Just twms -> previewHighlightedContent twms
    Nothing -> pure . str . massageForWidget . Text.unpack . decodeUtf8 $ selectedContents
  pure $
    selection &
      viewport Preview Both &
      padBottom Max &
      padRight Max &
      B.borderWithLabel (str selectedFileName) &
      padRight (Pad 1) &
      hLimitPercent 75

previewHighlightedContent :: Seq TextWithMatch -> RenderCtx (Widget Name)
previewHighlightedContent Seq.Empty = pure $ str " "
previewHighlightedContent twms = do
  rState <- viewing replaceState
  let broken = breakLines twms
      isMatch twm = case twm^.twmGroupL groupIndex of
                      Just gi -> Just gi == (rState >>= preview curGroupIndex)
                      Nothing -> False
      previewAttr twm = case twm^.twmGroupL groupIndex of
                          Just gi ->
                            if Just gi == (rState >>= preview curGroupIndex) then attrName "selectedMatch" else attrName "match"
                          _ -> mempty
      renderTwm :: TextWithMatch -> Widget Name
      renderTwm twm =
        let withVisibility widget = if isMatch twm then visibleRegion (Location (0,0)) (10, 10) widget else widget
         in twm &
              view content &
              decodeUtf8 &
              Text.unpack &
              massageForWidget &
              str &
              withAttr (previewAttr twm) &
              withVisibility
      renderLine :: [TextWithMatch] -> Widget Name
      renderLine [] = str " "
      renderLine twms' = hBox (map renderTwm twms')
  pure . vBox . map (renderLine . toList) . toList $ broken


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
replaceTabs '\n' = "\n"
replaceTabs '\t' = "    "
replaceTabs '\r' = " "
replaceTabs c
  | isSpace c = " "
  | otherwise = [c]
