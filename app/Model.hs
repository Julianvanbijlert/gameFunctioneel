module Model where
  -- | This module contains the data types
  --   which represent the state of the game
data Point = Point (Float, Float)
data Vector = Vector Point

data Player = Player Point Vector
data Enemy = SpaceShip Point Vector | --can also shoot
             Rock      Point Vector

data Border = Border Float Float --top y, bottom y

data Powerup = FastShot | DoubleBullet | Extralife 


data State = Running | Paused | GameOver
data Game = Game {}

data InfoToShow = InfoToShow{border :: Border, player :: Player, enemies :: [Enemy]} 
  
                | ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
              --  | ShowBorder Border
              --  | ShowPlayer Player
              --  | ShowEnemy Enemy
              --  | ShowListEnemy [Enemy]


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 500

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }



initialPlayer :: Player
initialPlayer = Player (Point(-300, 0)) (Vector(Point(0, 1)))

borders :: Border
borders = Border 340 (-340) --top y bottom y

startEnemies :: [Enemy]
startEnemies = [SpaceShip (Point(100, 100)) (Vector(Point(0,0))),
                SpaceShip (Point(10, 10)) (Vector(Point(0,0))),
                SpaceShip (Point(10, 100)) (Vector(Point(0,0))),
                Rock (Point(200, 200)) (Vector(Point(100, 0))) ]

initialState :: GameState
initialState = GameState (InfoToShow borders initialPlayer startEnemies)  0
