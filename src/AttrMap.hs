module AttrMap (module AttrMap) where

import Prelude (Int)
import Data.Monoid
import qualified Brick
import qualified Brick.Widgets.ProgressBar as Progress
import qualified Graphics.Vty as V

attrMap :: Brick.AttrMap
attrMap = Brick.attrMap V.defAttr
  [ (Progress.progressCompleteAttr, V.defAttr `V.withStyle` V.reverseVideo)
  , (input, V.defAttr `V.withForeColor` V.blue)
  , (error, V.defAttr `V.withForeColor` V.red `V.withStyle` V.reverseVideo)
  , (selectedFile, V.defAttr `V.withStyle` V.reverseVideo)
  , (focusSelectedFile, V.defAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (match, V.defAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (selectedMatch, V.defAttr `V.withForeColor` V.yellow `V.withStyle` V.reverseVideo)
  , (instructions, V.defAttr `V.withStyle` V.reverseVideo)
  , (placeholder, V.defAttr `V.withForeColor` V.rgbColor (150 :: Int) (150 :: Int) (150 :: Int))
  , (checked, V.defAttr `V.withStyle` V.reverseVideo)
  , (checked <> selected, Brick.fg V.blue `V.withStyle` V.reverseVideo)
  , (selected, Brick.fg V.blue)
  ]

input :: Brick.AttrName
input = Brick.attrName "input"

error :: Brick.AttrName
error = Brick.attrName "error"

selectedFile :: Brick.AttrName
selectedFile = Brick.attrName "selectedFile"

focusSelectedFile :: Brick.AttrName
focusSelectedFile = Brick.attrName "focusSelectedFile"

match :: Brick.AttrName
match = Brick.attrName "match"

selectedMatch :: Brick.AttrName
selectedMatch = Brick.attrName "selectedMatch"

instructions :: Brick.AttrName
instructions = Brick.attrName "instructions"

placeholder :: Brick.AttrName
placeholder = Brick.attrName "placeholder"

checked :: Brick.AttrName
checked = Brick.attrName "checked"

selected :: Brick.AttrName
selected = Brick.attrName "selected"
