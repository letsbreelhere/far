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

mkRegex :: String -> Maybe Regex
mkRegex = Regex.makeRegexM

findMatches :: Regex -> String -> [CaptureGroup]
findMatches r s = map toCaptureGroup (Regex.matchAll r s)

toCaptureGroup :: Regex.MatchArray -> CaptureGroup
toCaptureGroup ma = mapMaybe toMatch (toList ma `zip` [0..])
  where toMatch ((i, _), _) | i < 0 = Nothing
        toMatch ((i, l), c) = Just $ Match { _matchIndex=i, _matchLength=l, _captureIndex=c}

textWithMatches :: Regex -> String -> Seq TextWithMatch
textWithMatches r s =
  let cgs = findMatches r s
   in if null cgs
        then Seq.singleton (TextWithMatch s Nothing)
        else
          let withoutSuffix = snd $ foldl' (pairWithContent s) (0, Seq.empty) cgs
              lastMatchEnds = matchEnds (head $ last cgs)
              suffix = slice (lastMatchEnds, length s - lastMatchEnds) s
              suffixWithMatch = TextWithMatch suffix Nothing
           in Seq.filter (not . null . view content) (withoutSuffix |> suffixWithMatch)

slice :: (Int, Int) -> [a] -> [a]
slice (i, len) = take len . drop i

pairWithContent :: String -> (Int, Seq TextWithMatch) -> CaptureGroup -> (Int, Seq TextWithMatch)
pairWithContent s (i, acc) (m:_) = (m^.matchIndex+m^.matchLength, acc |> nextNonMatch |> nextMatch)
  where nextNonMatch = TextWithMatch (slice (i, (m^.matchIndex) - i) s) Nothing
        nextMatch = TextWithMatch (slice (m^.matchIndex, m^.matchLength) s) (Just $ m^.captureIndex)
pairWithContent _ _ [] = error "unexpected empty match"
