module Game where

import Logic
import Graphics.Gloss


data Player = Undefined
data Enemy = Spaceship Logic.Point Logic.Vector | --can also shoot
             Rock      Logic.Point Logic.Vector

data Powerup = FastShot | DoubleBullet | Extralife 


data State = Running | Paused | GameOver
data Game = Game {}

