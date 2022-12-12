{-# LANGUAGE RankNTypes #-}

module Util (module Util) where

import Types

import Control.Monad.State
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

equalify :: MonadState s m => (ASetter' s a -> b -> s -> s) -> Lens' s a -> b -> m ()
equalify op l v = modify (op l v)

infix 4 .=
(.=) :: MonadState s m => Lens' s a -> a -> m ()
(.=) = equalify (.~)

infix 4 +=
(+=) :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
(+=) = equalify (+~)

infix 4 -=
(-=) :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
(-=) = equalify (-~)

infix 4 %=
(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
(%=) = equalify (%~)

infix 4 ?=
(?=) :: MonadState s m => Lens' s (Maybe a) -> a -> m ()
(?=) = equalify (?~)

use :: MonadState s m => Getting a s a -> m a
use l = gets (^. l)

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
