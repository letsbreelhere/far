{-# LANGUAGE RankNTypes #-}

module LensUtil (module LensUtil) where

import Control.Monad.State
import Lens.Micro

equalify :: MonadState s m => (ASetter' s a -> b -> s -> s) -> Lens' s a -> b -> m ()
equalify op l v = modify (op l v)

(.=) :: MonadState s m => Lens' s a -> a -> m ()
(.=) = equalify (.~)

(+=) :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
(+=) = equalify (+~)

(-=) :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
(-=) = equalify (-~)

(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
(%=) = equalify (%~)

(?=) :: MonadState s m => Lens' s (Maybe a) -> a -> m ()
(?=) = equalify (?~)

use :: MonadState s m => Lens' s a -> m a
use l = gets (^. l)
