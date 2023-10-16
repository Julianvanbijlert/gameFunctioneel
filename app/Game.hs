module Game where

import Logic
import Graphics.Gloss


data Player = Undefined
data Enemy = Undefined

data State = Running | Paused | GameOver
data Game = Game {}

