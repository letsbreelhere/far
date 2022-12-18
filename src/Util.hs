{-# LANGUAGE RankNTypes #-}

module Util (module Util) where

import Types

import Control.Monad.State
import Data.ByteString.Char8 (elemIndices)
import Data.Sequence (Seq, (|>))
import Data.Text.Zipper (getText)
import Data.Text (Text)
import Lens.Micro
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as List
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Text.Regex.PCRE (Regex)
import Search (mkRegex, textWithMatches)

filterMSeq :: (a -> IO Bool) -> Seq a -> IO (Seq a)
filterMSeq p = foldM (\acc a -> p a >>= \b -> if b then pure (acc |> a) else pure acc) Seq.empty

nextName :: Name -> Name
nextName FileBrowser = FromInput
nextName FromInput = ToInput
nextName ToInput = FileBrowser
nextName Preview = error "No nextName for Preview"

prevName :: Name -> Name
prevName FromInput = FileBrowser
prevName FileBrowser = ToInput
prevName ToInput = FromInput
prevName Preview = error "No prevName for Preview"

selectionL ::
  (List.Splittable t, Traversable t, Semigroup (t e)) =>
  SimpleGetter (List.GenericList n t e) (Maybe e)
selectionL = to (fmap snd . List.listSelectedElement)

editorContentL :: SimpleGetter (Edit.Editor Text n) Text
editorContentL = Edit.editContentsL . to getText . to Text.concat

curFile :: SimpleGetter AppState (Maybe File)
curFile = to $ \s -> do
    (fName, fContents) <- s ^. matchedFiles.selectionL
    pure $ File fName fContents (elemIndices '\n' fContents)

compiledRegexL :: SimpleGetter AppState (Maybe Regex)
compiledRegexL = regexFrom .
  editorContentL .
  to Text.unpack .
  to (\t -> guard (not $ null t) >> mkRegex t)

textWithMatchesL :: SimpleGetter AppState (Maybe ([CaptureGroup], Seq TextWithMatch))
textWithMatchesL = to $ \s -> do
  regex <- s ^. compiledRegexL
  (_, selectedContents) <- s ^. matchedFiles . selectionL
  pure $ textWithMatches regex selectedContents
