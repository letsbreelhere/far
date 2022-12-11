module Util (module Util) where

import Control.Monad (foldM)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

filterMSeq :: (a -> IO Bool) -> Seq a -> IO (Seq a)
filterMSeq p = foldM (\acc a -> p a >>= \b -> if b then pure (acc |> a) else pure acc) Seq.empty
