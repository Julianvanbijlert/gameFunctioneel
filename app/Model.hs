module Model where
  -- | This module contains the data types
  --   which represent the state of the game
data Point = Point (Float, Float)
data Vector = Vector Point

data Player = Player Point Vector
data Enemy = Spaceship Point Vector | --can also shoot
             Rock      Point Vector

data Powerup = FastShot | DoubleBullet | Extralife 


data State = Running | Paused | GameOver
data Game = Game {}

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowPlayer Player
                | ShowRock


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 500

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState (ShowPlayer initialPlayer) 0

initialPlayer :: Player
initialPlayer = Player (Point(0, 0)) (Vector(Point(0, 1)))

