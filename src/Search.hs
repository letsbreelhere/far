module Search (module Search) where

import Data.TextWithMatch

import Data.ByteString (ByteString)
import Data.Foldable (Foldable(toList), find)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq(..), (|>))
import Lens.Micro
import Lens.Micro.Extras (view)
import Text.Regex.PCRE (Regex)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Text.Regex.PCRE as Regex
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as L
import Text.Read (readMaybe)

mkRegex :: String -> Maybe Regex
mkRegex = Regex.makeRegexM

findMatches :: Regex -> ByteString -> [CaptureGroup]
findMatches r s = zipWith (toCaptureGroup s) (Regex.matchAll r s) [0..]

toCaptureGroup :: ByteString -> Regex.MatchArray -> Int -> CaptureGroup
toCaptureGroup s ma groupIx =
  let mayMatches = nonEmpty $ mapMaybe toMatch (toList ma `zip` [0..])
   in case mayMatches of
        Just ms -> CaptureGroup { _matches=ms, _groupIndex = groupIx}
        Nothing -> error "Regex match array was unexpectedly empty"
  where toMatch ((i, _), _) | i < 0 = Nothing
        toMatch ((i, l), c) = Just $ Match { _matchStartIndex=i, _matchLength=l, _captureIndex=c, _matchContent=slice (i,l) s }

textWithMatches :: Regex -> ByteString -> Seq TextWithMatch
textWithMatches r s =
  let cgs = findMatches r s
   in if null cgs
        then Seq.singleton (TextWithMatch s Nothing)
        else
          let (_, _, withoutSuffix) = foldl' (pairWithContent s) (0, 0, Seq.empty) cgs
              lastMatchEnds = matchEnds (NE.head . view matches $ last cgs)
              suffix = slice (lastMatchEnds, ByteString.length s - lastMatchEnds) s
              suffixWithMatch = TextWithMatch suffix Nothing
           in Seq.filter (not . ByteString.null . view content) (withoutSuffix |> suffixWithMatch)

slice :: (Int, Int) -> ByteString -> ByteString
slice (i, len) = ByteString.take len . ByteString.drop i

matchEnds :: Match -> Int
matchEnds m = (m^.matchStartIndex) + (m^.matchLength)

pairWithContent :: ByteString -> (Int, Int, Seq TextWithMatch) -> CaptureGroup -> (Int, Int, Seq TextWithMatch)
pairWithContent s (maxCaptureIndex, matchCount, acc) cg = (m^.matchStartIndex+m^.matchLength, matchCount+1, acc |> nextNonMatch |> nextMatch)
  where m:|_ = cg^.matches
        nextNonMatch = TextWithMatch
                         { _content=slice (maxCaptureIndex, (m^.matchStartIndex) - maxCaptureIndex) s
                         , _captureGroup=Nothing
                         }
        nextMatch = TextWithMatch
                      { _content=slice (m^.matchStartIndex, m^.matchLength) s
                      , _captureGroup=Just cg
                      }

replacePatternRegex :: Regex
replacePatternRegex = Regex.makeRegex ("\\\\(\\d+)" :: String)

concatContents :: Seq TextWithMatch -> ByteString
concatContents = BS.concat . toList . fmap (view content)

replaceAll :: ByteString
           -> Seq TextWithMatch
           -> Maybe ByteString
replaceAll patternBS twms =
  let patternTwms = textWithMatches replacePatternRegex patternBS
   in BS.concat <$> mapM (replace patternTwms) (toList twms)

replace :: Seq TextWithMatch
        -- ^ The destination text, with capture placeholders of the form \1, \2, etc
        -> TextWithMatch
        -- ^ One piece of the source text, with or without matches
        -> Maybe ByteString
        -- ^ The destination text with its placeholders replaced
replace patternTwms twm =
  case twm^.captureGroup of
    Nothing -> Just (twm^.content)
    Just cg -> fmap BS.concat . sequence . toList $ fmap replaceCaptureGroup patternTwms
      where replaceCaptureGroup patTwm =
              case patTwm^.captureGroup of
                Nothing -> Just (patTwm^.content)
                Just patCg ->
                  do patternNumber <- readMaybe . BS.unpack . view matchContent . NE.last . view matches $ patCg
                     view matchContent <$> L.find (\m -> m^.captureIndex == patternNumber) (cg^.matches)

nthMatch :: TextWithMatch -> Int -> Maybe ByteString
nthMatch twm i =
  let mayMatch = find (\m -> m ^. captureIndex == i) (twm ^. captureGroup . _Just . matches . to toList)
   in fmap (view matchContent) mayMatch
