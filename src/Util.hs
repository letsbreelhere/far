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

mkBinTree :: Ord a => [a] -> BinTree a
mkBinTree [] = Tip
mkBinTree [a] = Branch a Tip Tip
mkBinTree as =
  let midPoint = length as `div` 2
      midElem = as !! midPoint
   in Branch
        midElem
        (mkBinTree (take midPoint as))
        (mkBinTree (drop (midPoint+1) as))

nearest :: (Num a, Ord a) => a -> BinTree a -> Maybe a
nearest = go Nothing
  where
    go mAcc _ Tip = mAcc
    go mAcc target (Branch a' l r) =
      let mAcc' = newBest target a' mAcc
       in case compare target a' of
            EQ -> Just target
            LT -> go mAcc' target l
            GT -> go mAcc' target r
    newBest target cur (Just candidate)
      | abs (target-candidate) < abs (target-cur) = Just candidate
    newBest _ cur _ = Just cur

nearestLT :: (Ord a) => a -> BinTree a -> Maybe a
nearestLT _ Tip = Nothing
nearestLT target (Branch x left right)
  | x < target = case right of
      Tip -> Just x
      _ -> case nearestLT target right of
        Nothing -> Just x
        Just y -> Just y
  | otherwise = nearestLT target left

countLT :: (Ord a) => a -> BinTree a -> Int
countLT target = sum . fmap (\candidate -> if candidate < target then 1 else 0)
