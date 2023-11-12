module View where
  -- | This module defines how to turn
  --   the game state into a picture

import Graphics.Gloss hiding (Point)
import Model
    ( Heart(..),
      Point(..),
      Border(..),
      Bullet(..),
      Enemy(..),
      Player(..),
      State(..),
      InfoToShow(..),
      GameState(GameState, infoToShow, state, score, hScores),
      screenw,
      screenh, )

import Data.List
import Data.Ord (comparing)



view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@(GameState {infoToShow = i, score = sc, hScores =  hs}) = case state gstate of
  Running  -> case infoToShow gstate of
            ShowNothing           -> blank
            InfoToShow {}         -> showInfoToShow i sc
            ShowANumber n         -> color green (text (show n))
            ShowAChar   c         -> color green (text [c])
  Paused   -> pause
  GameOver -> case infoToShow gstate of
                  InfoToShow {} -> gameOver
                  ShowHighScores        -> showHighScores $ orderList hs
  Dead     -> dead gstate

showInfoToShow :: InfoToShow -> Int -> Picture
showInfoToShow (InfoToShow {border = b, player = p, enemies = xs, bullets = bs}) sc = Pictures [showBorder b
                                                                                               , showPlayer p
                                                                                               , showListEnemies xs
                                                                                               , showBullets bs
                                                                                               , showScore sc
                                                                                               , showLives p]

showPlayer :: Player -> Picture
showPlayer (Player (Point(x, y)) _ _) = let (xSize, ySize) = (30, 10) in color green (Polygon [(x, y - ySize), (x, y + ySize), (x + xSize, y)] ) --triangle

showListEnemies :: [Enemy] -> Picture
showListEnemies [] = blank
showListEnemies e = Pictures $ map showEnemy e

showEnemy :: Enemy -> Picture
-- if the enemy is death it makes a explosion
showEnemy e@(Rock {enemyLives = []})               = showExplosion e
showEnemy e@(SpaceShip {enemyLives = []})          = showExplosion e
showEnemy e@(Jet {enemyLives = []})                = showExplosion e
showEnemy e@(MotherShip {enemyLives = []})         = showExplosion e
showEnemy  (Rock {enemyPos = (Point(x, y))})       = let (xSize, ySize) = (17, 10) in color white (Polygon [(x, y+(ySize*2)), (x+xSize, y+ySize), (x+xSize, y-ySize), (x, y-(ySize*2)), (x-xSize, y-ySize), (x-xSize, y+ySize)])
showEnemy  (SpaceShip {enemyPos = (Point(x, y))})  = let size = 10 in color red (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)])  --square
showEnemy  (Jet {enemyPos = (Point(x, y))})        = let size = 7 in color blue (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)])  --square
showEnemy  (MotherShip {enemyPos = (Point(x, y))}) = let size = 20 in color green (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)]) --square

showExplosion :: Enemy -> Picture
showExplosion  (Rock {enemyPos = p, frame = h})       = let (xSize, ySize) = (9,17) in makeExplosion p h xSize ySize
showExplosion  (SpaceShip {enemyPos = p, frame = h})  = let (xSize, ySize) = (4,10) in makeExplosion p h xSize ySize
showExplosion  (Jet {enemyPos = p, frame = h})        = let (xSize, ySize) = (2,7) in makeExplosion p h xSize ySize
showExplosion  (MotherShip {enemyPos = p, frame = h}) = let (xSize, ySize) = (9,20) in makeExplosion p h xSize ySize

makeExplosion :: Point -> Float -> Float -> Float -> Picture
-- makes an bigger explosion based on how many frames the enemy has been death
makeExplosion (Point (x,y)) frame xSize ySize = Pictures [color orange (Polygon [(x-xSize-sizePerFrame, y-ySize-sizePerFrame), (x-xSize-sizePerFrame, y+ySize+sizePerFrame), (x+ySize+sizePerFrame, y)])
                                                          , color orange (Polygon [(x+xSize+sizePerFrame, y-ySize-sizePerFrame), (x+xSize+sizePerFrame, y+ySize+sizePerFrame), (x-ySize-sizePerFrame, y)])]
                where sizePerFrame = let growthSize = 10 in frame/growthSize

showBullets :: [Bullet] -> Picture
showBullets []      = blank
showBullets (x: xs) = Pictures [showBullet x, showBullets xs]

showBullet :: Bullet -> Picture
showBullet (EnemyBullet (Point(x, y)) _)  = let (xSize, ySize) = (2,1) in color yellow (Polygon [(x - xSize, y -ySize),(x + xSize, y - ySize), (x + xSize, y + ySize), (x - xSize, y + ySize)])
showBullet (PlayerBullet (Point(x, y)) _) = let (xSize, ySize) = (2,1) in color white (Polygon [(x - xSize, y -ySize),(x + xSize, y - ySize), (x + xSize, y + ySize), (x - xSize, y + ySize)])

showBorder :: Border -> Picture
showBorder (Border ytop ybot) = pictures [ color green (Polygon [(sw, sh), (sw, ytop), (-sw , ytop), (-sw, sh) ]),
                                           color green (Polygon [(sw, -sh), (sw, ybot), (-sw , ybot), (-sw, -sh) ]) ]
                                            where sw = Model.screenw  --helft van screenwidth
                                                  sh = Model.screenh  --helft van screenheight

showScore :: Int -> Picture
showScore i = color white (Translate (-50) (Model.screenh - 50) (Scale 0.3 0.3 (text (show i))))

--we made this player instead of putting it in the game in case we wanted to add multiplayer
showLives :: Player -> Picture
showLives (Player (Point(x, y)) _ h) = showLive (Point (0, 0)) (reverse h)

showLive :: Model.Point -> [Heart] -> Picture
showLive _ [] = blank
showLive p@(Point (x, y)) (Heart : xs) = Pictures [drawHeart p, showLive (Point (x + 50, y)) xs]
showLive p@(Point (x, y)) (Shield : xs) = Pictures [drawShield p, showLive (Point (x + 50, y)) xs]

drawHeart :: Model.Point -> Picture
drawHeart (Point (x, y)) = Translate (-Model.screenw + 50 + x) (Model.screenh - 50 + y) heart
  --color white (Translate (-100) 300 (Scale 0.3 0.3 (text (show (length p)))))

--https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.vecteezy.com%2Fpng%2F12658366-heart-shaped-love-icon-symbol-for-pictogram-app-website-logo-or-graphic-design-element-pixel-art-style-illustration-format-png&psig=AOvVaw3I1hrWNnEue6ymB1XQF6uH&ust=1698229669669000&source=images&cd=vfe&ved=0CBEQjRxqFwoTCPCdnNS8joIDFQAAAAAdAAAAABAE
heart :: Picture
heart = color white (Scale 0.2 0.2 (text (show "<3")))

drawShield :: Model.Point -> Picture
drawShield (Point (x, y)) = Translate (-Model.screenw + 50 + x) (Model.screenh - 50 + y) shield

--Dall-e
shield :: Picture
shield = color white (Scale 0.2 0.2 (text (show "U")))

pause :: Picture
pause = Pictures [showContinue, showExit]
                where showContinue = Translate 0 (screenh * 0.5) (textBox "Continue")
                      showExit     = Translate 0 (-screenh * 0.5) (textBox "Exit")

gameOver :: Picture
gameOver = Pictures [showStart, showHighScore]
                where showStart     = Translate 0 (screenh * 0.5) (textBox "Start game")
                      showHighScore = Translate 0 (-screenh * 0.5) (textBox "High Scores")

textBox :: String -> Picture
textBox s = Pictures [box, text]
          where box = Color white $ rectangleWire (screenw * 0.5) (screenh * 0.2)
                text = Color white $ Scale 0.2 0.2 $ Translate (-(screenw * 0.1 * fromIntegral (length s))) (-(screenh * 0.2)) $ Text s

dead :: GameState -> Picture
dead g@(GameState i t s sc _ _)= Pictures [score, shownew, showHome]
        where score        = showInfoToShow i sc
              shownew      = Translate 0 (screenh * 0.5) (textBox "New Game")
              showHome     = Translate 0 (-screenh * 0.5) (textBox "Home")


{-
getHighScores :: IO Picture
getHighScores = do 
  config <- readFile "Scores.txt"
  let f:s:t:fo:fi:_ = lines config
  return (showHighScores f s t fo fi)
  -}

--showHighScores :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Picture
--showHighScores Nothing Nothing Nothing Nothing Nothing = blank


--inefficient maar werkt :D
showHighScores :: [String]-> Picture
showHighScores (f: s: t: fo: fi: _) = Pictures [ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, scoreToPic (-0.1) fo, scoreToPic (-0.3) fi, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: t: fo:  _)    = Pictures [ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, scoreToPic (-0.1) fo, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: t: _)         = Pictures [ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: _)            = Pictures [ scoreToPic 0.5 f, scoreToPic 0.3 s, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: _)               = Pictures [ scoreToPic 0.5 f,  Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores []                   = Translate 0 (-screenh * 0.6) (textBox "Home")
  

scoreToPic :: Float -> String -> Picture
scoreToPic y s = Translate xpos  ypos (maybeToScore s)
  where xpos = 0
        ypos = screenh * y

maybeToScore :: String -> Picture
--maybeToScore Nothing = blank
maybeToScore s = Color white $ Scale scaler scaler $ Translate lengthString pos $ Text s
  where scaler = 0.2
        lengthString = -(screenw * 0.1 * fromIntegral (length s))
        pos = -(screenh * scaler)

orderList :: [String] -> [String]
orderList = sortBy compareTwo

compareTwo :: String -> String -> Ordering
compareTwo a b = compare (read b :: Int) (read a :: Int)