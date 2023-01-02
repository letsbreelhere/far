module Widgets.Preview (previewPane) where

import Data.RenderCtx
import Data.TextWithMatch
import Types
import Util

import Brick
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text.Encoding (decodeUtf8')
import Lens.Micro
import Lens.Micro.Extras (view, preview)
import qualified Brick.Widgets.Border as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

errorLine :: Widget Name
errorLine = withAttr (attrName "error") $ str "[This file contains invalid characters and will not be displayed.]"

previewPane :: RenderCtx (Widget Name)
previewPane = do
  (selectedFileName, selectedContents) <- viewing (matchedFiles . selectionL . _Just)
  mayTwms <- viewing textWithMatchesL
  selection <- case mayTwms of
    Just twms -> previewHighlightedContent twms
    Nothing ->
      pure $ case decodeUtf8' selectedContents of
        Left _ -> errorLine
        Right t -> str . massageForWidget . Text.unpack $ t
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
      attemptDecode :: TextWithMatch -> Maybe String
      attemptDecode twm = case decodeUtf8' . view content $ twm of
                            Left _ -> Nothing
                            Right c -> Just (Text.unpack c)
      renderTwm :: TextWithMatch -> Maybe (Widget Name)
      renderTwm twm = do
        decoded <- attemptDecode twm
        pure . withAttr (previewAttr twm) . (if isMatch twm then visibleRegion (Location (0,0)) (10, 10) else id) . str . massageForWidget $ decoded
      renderLine :: [TextWithMatch] -> Maybe (Widget Name)
      renderLine [] = Just (str " ")
      renderLine twms' = hBox <$> mapM renderTwm twms'
  pure . maybe errorLine vBox $ mapM (renderLine . toList) (toList broken)


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
