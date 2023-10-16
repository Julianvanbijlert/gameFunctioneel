module Rendering where

import Game
import Graphics.Gloss

stateAction :: State -> undefined
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined

