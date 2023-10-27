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
numberOfSecsBetweenActions = 0.3



data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , state :: State
                 , score :: Int
                 , rndGen :: StdGen
                 }

screenWidths :: Float
screenWidths = 700

screenHeights :: Float
screenHeights = 400

screenw :: Float
screenw = screenWidths / 2

screenh :: Float
screenh = screenHeights / 2

initialPlayer :: Player
<<<<<<< HEAD
initialPlayer = Player (Point(-300, 0)) (Vector(10, 10)) [Shield, Heart, Heart, Heart]
=======
initialPlayer = Player (Point (-300, 0)) (Vector (0, 10)) [Heart, Heart, Heart, Shield]
>>>>>>> 1d6908c39c2881aa8b45f42d89caf082a6880451

borders :: Border
borders = Border (screenh - 10) (-screenh + 10) --top y bottom y

startEnemies :: [Enemy]
<<<<<<< HEAD
startEnemies = [SpaceShip (Point(200, 300)) (Vector(-2, -1)) [Heart],
                Rock      (Point(200, 200)) (Vector(-2, -1)) [Heart] ]
=======
startEnemies = [ ]
>>>>>>> 1d6908c39c2881aa8b45f42d89caf082a6880451

initialState :: StdGen -> GameState
initialState = GameState (InfoToShow borders initialPlayer startEnemies [] )  0 Running 0 --nog een random stdgen nodig
