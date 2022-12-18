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

filterMSeq :: (a -> IO Bool) -> Seq a -> IO (Seq a)
filterMSeq p = foldM (\acc a -> p a >>= \b -> if b then pure (acc |> a) else pure acc) Seq.empty

nextName :: Name -> Name
nextName n
  | n < maxBound = toEnum . succ . fromEnum $ n
  | otherwise = minBound

prevName :: Name -> Name
prevName n
  | n > minBound = toEnum . pred . fromEnum $ n
  | otherwise = maxBound

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
