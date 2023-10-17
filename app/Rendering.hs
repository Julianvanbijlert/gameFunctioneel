module Rendering where

import Game
import Graphics.Gloss

stateAction :: State -> Picture --niet goed
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined

