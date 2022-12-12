{-# LANGUAGE RankNTypes #-}

module Util (module Util) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Monad.State
import Lens.Micro

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
