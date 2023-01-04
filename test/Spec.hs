import Search

import qualified Data.ByteString.Char8 as BS
import Data.TextWithMatch
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Regex.PCRE as Regex
import Data.Foldable (Foldable(toList))
import Lens.Micro.Extras (view)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ textWithMatchesTests
  , replaceAllTests
  ]

vowelRegex = Regex.makeRegex ("[^aeiou]([aeiou]+)" :: String)

textWithMatchesTests :: TestTree
textWithMatchesTests = testGroup "textWithMatches"
  [
    testCase "it handles capture groups" $ do
      let twms = toList $ textWithMatches vowelRegex "ffoobarbazquux"
      map (view content) twms @?= ["f", "foo", "ba", "r", "ba", "z", "quu", "x"]
      head twms ^. captureGroup @?= Nothing
      let fooMatches = (twms !! 1) ^. captureGroup . to fromJust . matches
      NE.length fooMatches @?= 2
      NE.head fooMatches @?= Match 1 3 0 "foo"
      head (NE.tail fooMatches) @?= Match 2 2 1 "oo"
  ]

replaceAllTests :: TestTree
replaceAllTests = testGroup "replaceAll"
  [
    testCase "it does no replacement on non-matches" $ do
      let twms = textWithMatches vowelRegex "ffoobarbazquux"
          replaceResults = replaceAll "p\\1" twms
      replaceResults @?= Right "fpooparpazpuux"
  ]
