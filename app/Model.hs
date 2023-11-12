module Model where
import System.Random
  -- | This module contains the data types
  --   which represent the state of the game
newtype Point = Point (Float, Float)
newtype Vector = Vector (Float, Float)

data Player = Player       { pos :: Point
                           , dir :: Vector
                           , lives :: [Heart] }
data Enemy =  SpaceShip    { enemyPos :: Point
                           , enemyDir :: Vector
                           , enemyLives :: [Heart]
                           , frame :: Float }| --can also shoot
              Rock         { enemyPos :: Point
                           , enemyDir :: Vector
                           , enemyLives :: [Heart]
                           , frame :: Float }|
              Jet          { enemyPos :: Point
                           , enemyDir :: Vector
                           , enemyLives :: [Heart]
                           , frame :: Float }|
              MotherShip   { enemyPos :: Point
                           , enemyDir :: Vector
                           , enemyLives :: [Heart]
                           , frame :: Float }


data Bullet = EnemyBullet  { bulletPos :: Point
                           , bulletDir :: Vector }|
              PlayerBullet { bulletPos :: Point
                           , bulletDir :: Vector }

data Heart =  Heart |
              Shield


data Border = Border Float Float --top y, bottom y

data Powerup = FastShot | DoubleBullet | Extralife


data State = Running | Paused | GameOver | Dead
data Game = Game {}

data InfoToShow = InfoToShow { border :: Border
                             , player :: Player
                             , enemies :: [Enemy]
                             , bullets :: [Bullet] }
                            | ShowNothing
                            | ShowANumber Int
                            | ShowAChar   Char
                            | ShowHighScores


numberOfSecsBetweenActions :: Float
numberOfSecsBetweenActions = 1



data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , state :: State
                 , score :: Int
                 , hScores :: [String]
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

beginPoint :: Point
beginPoint = Point (-300, 0)

playerVector :: Vector
playerVector = Vector (10, 10)

initialPlayer :: Player
initialPlayer = Player beginPoint playerVector [Shield, Heart, Heart, Heart]

borders :: Border
borders = let distanceTillBorder = 10 in Border (screenh - distanceTillBorder) (-screenh + distanceTillBorder) --top y bottom y

-- Enemies will be spawned in the beginning
-- Examples: [Rock (Point (screenw-10, 100)) (Vector (-1,0)) [Heart] 0
--                 , SpaceShip (Point (screenw-10, 50)) (Vector (-1, -(1/13))) [Heart, Heart, Heart] 0
--                 , Jet (Point (screenw-10, -50)) (Vector (-1, 1/13)) [Heart, Heart, Heart] 0
--                 , MotherShip (Point (screenw-10, -100)) (Vector (0, 1)) [Heart, Heart, Heart, Heart, Heart] 0
--                 ]
startEnemies :: [Enemy]
startEnemies = [ ]

initialState :: StdGen -> [String] -> GameState
initialState s hs = GameState (InfoToShow borders initialPlayer startEnemies [] )  0 Running 0 hs s --nog een random stdgen nodig
