module AttrMap (module AttrMap) where

import Prelude()
import qualified Brick
import qualified Brick.Widgets.ProgressBar as Progress
import qualified Graphics.Vty as V

attrMap :: Brick.AttrMap
attrMap = Brick.attrMap V.defAttr
  [ (Progress.progressCompleteAttr, V.currentAttr `V.withStyle` V.reverseVideo)
  , (input, V.currentAttr `V.withForeColor` V.blue)
  , (error, V.currentAttr `V.withForeColor` V.red)
  , (selectedFile, V.currentAttr `V.withStyle` V.reverseVideo)
  , (focusSelectedFile, V.currentAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (match, V.currentAttr `V.withForeColor` V.blue `V.withStyle` V.reverseVideo)
  , (selectedMatch, V.currentAttr `V.withForeColor` V.yellow `V.withStyle` V.reverseVideo)
  , (instructions, V.currentAttr `V.withStyle` V.reverseVideo)
  , (placeholder, V.currentAttr `V.withForeColor` V.rgbColor 150 150 150)
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
