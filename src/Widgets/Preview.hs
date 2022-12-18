module Widgets.Preview (previewPane) where

import Data.RenderCtx
import Types
import Util

import Brick
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Lens.Micro
import Lens.Micro.Extras (view, preview)
import qualified Brick.Widgets.Border as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

previewPane :: RenderCtx (Widget Name)
previewPane = do
  mayTwms <- viewing textWithMatchesL
  (selectedFileName, selectedContents) <- viewing (matchedFiles . selectionL . _Just)
  selection <- case mayTwms of
    Just (cgs, twms) -> previewHighlightedContent cgs twms
    Nothing -> pure . str . massageForWidget . Text.unpack . decodeUtf8 $ selectedContents
  pure $
    selection &
      viewport Preview Both &
      padBottom Max &
      padRight Max &
      B.borderWithLabel (str selectedFileName) &
      padRight (Pad 1) &
      hLimitPercent 75

previewHighlightedContent :: [CaptureGroup] -> Seq TextWithMatch -> RenderCtx (Widget Name)
previewHighlightedContent _ Seq.Empty = pure $ str " "
previewHighlightedContent _ twms = do
  rState <- viewing replaceState
  let broken = breakLines twms
      isMatch twm = case twm^.twmGroupL groupIndex of
                      Just gi -> Just gi == (rState >>= preview curGroupIndex)
                      Nothing -> False
      previewAttr twm = case twm^.twmGroupL groupIndex of
                          Just gi ->
                            if Just gi == (rState >>= preview curGroupIndex) then attrName "selectedMatch" else attrName "match"
                          _ -> mempty
      attemptDecode :: TextWithMatch -> Maybe String
      attemptDecode twm = case decodeUtf8' . view content $ twm of
                            Left _ -> Nothing
                            Right c -> Just (Text.unpack c)
      renderTwm :: TextWithMatch -> Maybe (Widget Name)
      renderTwm twm = do
        decoded <- attemptDecode twm
        pure . withAttr (previewAttr twm) . (if isMatch twm then visibleRegion (Location (0,0)) (10, 10) else id) . str . massageForWidget $ decoded
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
