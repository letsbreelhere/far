{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Data.Zipper (module Data.Zipper) where

import Lens.Micro.TH (makeLenses)
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq

data Zipper a = Zipper
  { _before :: Seq a
  , _zipCursor :: a
  , _after :: Seq a
  }
  deriving (Show, Functor)
makeLenses ''Zipper

mkZipper :: Seq a -> Maybe (Zipper a)
mkZipper (a:<|as) = Just (Zipper Seq.empty a as)
mkZipper Seq.Empty = Nothing

cursorNext :: Zipper a -> Maybe (Zipper a)
cursorNext (Zipper bs cur (a:<|as)) = Just (Zipper (bs|>cur) a as)
cursorNext _ = Nothing

cursorPrev :: Zipper a -> Maybe (Zipper a)
cursorPrev (Zipper (b:<|bs) cur as) = Just (Zipper bs b (cur<|as))
cursorPrev _ = Nothing

toSeq :: Zipper a -> Seq a
toSeq (Zipper bs cur as) = bs <> (cur <| as)
