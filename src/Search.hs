module Search (mkRegex, findMatches, textWithMatches) where

import Types

import Data.ByteString (ByteString)
import Data.Foldable (Foldable(toList))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq(..), (|>))
import Lens.Micro
import Lens.Micro.Extras (view)
import Text.Regex.PCRE (Regex)
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Text.Regex.PCRE as Regex

mkRegex :: String -> Maybe Regex
mkRegex = Regex.makeRegexM

findMatches :: Regex -> ByteString -> [CaptureGroup]
findMatches r s = zipWith toCaptureGroup (Regex.matchAll r s) [0..]

toCaptureGroup :: Regex.MatchArray -> Int -> CaptureGroup
toCaptureGroup ma ix =
  let ms = fromList $ mapMaybe toMatch (toList ma `zip` [0..])
   in CaptureGroup { _matches=ms, _groupIndex = ix}
  where toMatch ((i, _), _) | i < 0 = Nothing
        toMatch ((i, l), c) = Just $ Match { _matchStartIndex=i, _matchLength=l, _captureIndex=c}

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
