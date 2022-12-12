module Search (mkRegex, findMatches, textWithMatches) where

import Types

import Lens.Micro
import qualified Text.Regex.PCRE as Regex
import Text.Regex.PCRE (Regex)
import Data.Foldable (Foldable(toList))
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Lens.Micro.Extras (view)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

mkRegex :: String -> Maybe Regex
mkRegex = Regex.makeRegexM

findMatches :: Regex -> ByteString -> [CaptureGroup]
findMatches r s = map toCaptureGroup (Regex.matchAll r s)

toCaptureGroup :: Regex.MatchArray -> CaptureGroup
toCaptureGroup ma = mapMaybe toMatch (toList ma `zip` [0..])
  where toMatch ((i, _), _) | i < 0 = Nothing
        toMatch ((i, l), c) = Just $ Match { _matchIndex=i, _matchLength=l, _captureIndex=c}

textWithMatches :: Regex -> ByteString -> Seq TextWithMatch
textWithMatches r s =
  let cgs = findMatches r s
   in if null cgs
        then Seq.singleton (TextWithMatch s Nothing Nothing)
        else
          let (_, _, withoutSuffix) = foldl' (pairWithContent s) (0, 0, Seq.empty) cgs
              lastMatchEnds = matchEnds (head $ last cgs)
              suffix = slice (lastMatchEnds, ByteString.length s - lastMatchEnds) s
              suffixWithMatch = TextWithMatch suffix Nothing Nothing
           in Seq.filter (not . ByteString.null . view content) (withoutSuffix |> suffixWithMatch)

slice :: (Int, Int) -> ByteString -> ByteString
slice (i, len) = ByteString.take len . ByteString.drop i

matchEnds :: Match -> Int
matchEnds m = (m^.matchIndex) + (m^.matchLength)

pairWithContent :: ByteString -> (Int, Int, Seq TextWithMatch) -> CaptureGroup -> (Int, Int, Seq TextWithMatch)
pairWithContent s (maxCaptureIndex, matchCount, acc) (m:_) = (m^.matchIndex+m^.matchLength, matchCount+1, acc |> nextNonMatch |> nextMatch)
  where nextNonMatch = TextWithMatch
                         { _content=slice (maxCaptureIndex, (m^.matchIndex) - maxCaptureIndex) s
                         , _mayCaptureIndex=Nothing
                         , _mayMatchIndex=Nothing
                         }
        nextMatch = TextWithMatch
                      { _content=slice (m^.matchIndex, m^.matchLength) s
                      , _mayCaptureIndex=Just $ m^.captureIndex
                      , _mayMatchIndex=Just matchCount
                      }
pairWithContent _ _ [] = error "unexpected empty match"
