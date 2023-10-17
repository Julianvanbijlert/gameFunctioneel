module Model where
  -- | This module contains the data types
  --   which represent the state of the game
  
data Point = Pt Float Float
data Vector = Vec Float Float

data Player = Undefined
data Enemy = Spaceship Point Vector | --can also shoot
             Rock      Point Vector

data Powerup = FastShot | DoubleBullet | Extralife 


data State = Running | Paused | GameOver
data Game = Game {}

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0

