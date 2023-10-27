module Model where
import System.Random
  -- | This module contains the data types
  --   which represent the state of the game
newtype Point = Point (Float, Float)
newtype Vector = Vector (Float, Float)

data Player = Player       Point Vector [Heart]
data Enemy =  SpaceShip    Point Vector [Heart]| --can also shoot
              Rock         Point Vector [Heart]

data Bullet = EnemyBullet  Point Vector |
              PlayerBullet Point Vector

data Heart =  Heart |
              Shield

data Border = Border Float Float --top y, bottom y

data Powerup = FastShot | DoubleBullet | Extralife


data State = Running | Paused | GameOver
data Game = Game {}

data InfoToShow = InfoToShow{border :: Border, player :: Player, enemies :: [Enemy], bullets :: [Bullet]}

                | ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
              --  | ShowBorder Border
              --  | ShowPlayer Player
              --  | ShowEnemy Enemy
              --  | ShowListEnemy [Enemy]


numberOfSecsBetweenActions :: Float
numberOfSecsBetweenActions = 0.0002

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , state :: State
                 , score :: Int
                 , rndGen :: StdGen
                 }



initialPlayer :: Player
initialPlayer = Player (Point (-300, 0)) (Vector (0, 10)) [Heart, Heart, Heart, Shield]

borders :: Border
borders = Border 340 (-340) --top y bottom y

startEnemies :: [Enemy]
startEnemies = [ ]

initialState :: StdGen -> GameState
initialState = GameState (InfoToShow borders initialPlayer startEnemies [] )  0 Running 0 --nog een random stdgen nodig
