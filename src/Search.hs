module Search (mkRegex, findMatches) where

import Types

import qualified Text.Regex.PCRE as Regex
import Text.Regex.PCRE (Regex)
import Data.Foldable (Foldable(toList))
import Data.Maybe (mapMaybe)

mkRegex :: String -> Regex
mkRegex = Regex.makeRegex

findMatches :: Regex -> String -> [CaptureGroup]
findMatches r s = map toCaptureGroup (Regex.matchAll r s)

toCaptureGroup :: Regex.MatchArray -> CaptureGroup
toCaptureGroup ma = mapMaybe toMatch (toList ma `zip` [0..])
  where toMatch ((i, _), _) | i < 0 = Nothing
        toMatch ((i, l), c) = Just $ Match { _matchIndex=i, _matchLength=l, _captureIndex=c}
