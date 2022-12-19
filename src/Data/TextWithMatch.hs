{-# LANGUAGE TemplateHaskell #-}

module Data.TextWithMatch (module Data.TextWithMatch) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro.TH (makeLenses)

data Match = Match
  { _matchStartIndex :: Int
  , _matchLength :: Int
  , _captureIndex :: Int
  , _matchContent :: ByteString
  }
  deriving (Show, Eq)
makeLenses ''Match

data CaptureGroup = CaptureGroup
  { _matches :: NonEmpty Match
  , _groupIndex :: Int
  }
  deriving (Show, Eq)
makeLenses ''CaptureGroup

data TextWithMatch = TextWithMatch
  { _content :: ByteString
  , _captureGroup :: Maybe CaptureGroup
  }
  deriving (Show, Eq)
makeLenses ''TextWithMatch
