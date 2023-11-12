module View where
  -- | This module defines how to turn
  --   the game state into a picture

import Graphics.Gloss hiding (Point)
import Model

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
                  ShowHighScores        -> showHighScores hs
  Dead     -> dead gstate

showInfoToShow :: InfoToShow -> Int -> Picture
showInfoToShow (InfoToShow {border = b, player = p, enemies = xs, bullets = bs}) sc = Pictures [showObject b
                                                                                               , showObject p
                                                                                               , showObjects xs
                                                                                               , showObjects bs
                                                                                               , showObject sc
                                                                                               , showLives p]

class ShowObject a where
  showObject :: a -> Picture
  showObjects :: [a] -> Picture
instance ShowObject Player where
  showObject (Player {pos = (Point(x, y))}) = let (xSize, ySize) = (30, 10) in color green (Polygon [(x, y - ySize), (x, y + ySize), (x + xSize, y)] ) --triangle
  showObjects p = blank
instance ShowObject Enemy where 
-- if the enemy is death it makes a explosion
  showObject  (Rock {enemyPos = p, enemyLives = [], frame = h})       = let (xSize, ySize) = (9,17) in makeExplosion p h xSize ySize
  showObject  (SpaceShip {enemyPos = p, enemyLives = [], frame = h})  = let (xSize, ySize) = (4,10) in makeExplosion p h xSize ySize
  showObject  (Jet {enemyPos = p, enemyLives = [], frame = h})        = let (xSize, ySize) = (2,7) in makeExplosion p h xSize ySize
  showObject  (MotherShip {enemyPos = p, enemyLives = [], frame = h}) = let (xSize, ySize) = (9,20) in makeExplosion p h xSize ySize
  showObject  (Rock {enemyPos = (Point(x, y))})       = let (xSize, ySize) = (17, 10) in color white (Polygon [(x, y+(ySize*2)), (x+xSize, y+ySize), (x+xSize, y-ySize), (x, y-(ySize*2)), (x-xSize, y-ySize), (x-xSize, y+ySize)])
  showObject  (SpaceShip {enemyPos = (Point(x, y))})  = let size = 10 in color red (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)])  --square
  showObject  (Jet {enemyPos = (Point(x, y))})        = let size = 7 in color blue (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)])  --square
  showObject  (MotherShip {enemyPos = (Point(x, y))}) = let size = 20 in color green (Polygon [(x-size, y-size),(x+size, y+size), (x-size, y+size), (x+size, y-size)]) --square
  showObjects [] = blank
  showObjects e = Pictures $ map showObject e
instance ShowObject Bullet where 
  showObject (EnemyBullet (Point(x, y)) _)  = let (xSize, ySize) = (2,1) in color yellow (Polygon [(x - xSize, y -ySize),(x + xSize, y - ySize), (x + xSize, y + ySize), (x - xSize, y + ySize)])
  showObject (PlayerBullet (Point(x, y)) _) = let (xSize, ySize) = (2,1) in color white (Polygon [(x - xSize, y -ySize),(x + xSize, y - ySize), (x + xSize, y + ySize), (x - xSize, y + ySize)])
  showObjects [] = blank
  showObjects b = Pictures $ map showObject b
instance ShowObject Border where
  showObject (Border ytop ybot) = pictures [ color green (Polygon [(sw, sh), (sw, ytop), (-sw , ytop), (-sw, sh) ]),
                                           color green (Polygon [(sw, -sh), (sw, ybot), (-sw , ybot), (-sw, -sh) ]) ]
                                            where sw = Model.screenw  --helft van screenwidth
                                                  sh = Model.screenh  --helft van screenheight
  showObjects b = blank
instance ShowObject Int where
  showObject i = color white (Translate (-50) (Model.screenh - 50) (Scale 0.3 0.3 (text (show i))))
  showObjects sc = blank

makeExplosion :: Point -> Float -> Float -> Float -> Picture
-- makes an bigger explosion based on how many frames the enemy has been death
makeExplosion (Point (x,y)) frame xSize ySize = Pictures [color orange (Polygon [(x-xSize-sizePerFrame, y-ySize-sizePerFrame), (x-xSize-sizePerFrame, y+ySize+sizePerFrame), (x+ySize+sizePerFrame, y)])
                                                          , color orange (Polygon [(x+xSize+sizePerFrame, y-ySize-sizePerFrame), (x+xSize+sizePerFrame, y+ySize+sizePerFrame), (x-ySize-sizePerFrame, y)])]
                where sizePerFrame = let growthSize = 10 in frame/growthSize

--we made this player instead of putting it in the game in case we wanted to add multiplayer
showLives :: Player -> Picture
showLives (Player {lives = h}) = showLive p (reverse h)
            where p = Point (0, 0)

showLive :: Model.Point -> [Heart] -> Picture
showLive _ [] = blank
showLive p@(Point (x, y)) (Heart : xs) = Pictures [drawHeart p, showLive (Point (x + widthHeart, y)) xs]
            where widthHeart = 50
showLive p@(Point (x, y)) (Shield : xs) = Pictures [drawShield p, showLive (Point (x + widthHeart, y)) xs]
            where widthHeart = 50

drawHeart :: Model.Point -> Picture
drawHeart (Point (x, y)) = Translate xloc yloc heart
            where xloc = -Model.screenw + 50 + x
                  yloc = Model.screenh - 50 + y

heart :: Picture
heart = color white (Scale scaler scaler (text (show "<3")))
  where scaler = 0.2

drawShield :: Model.Point -> Picture
drawShield (Point (x, y)) = Translate xloc yloc shield
          where xloc = -Model.screenw + 50 + x
                yloc = Model.screenh - 50 + y


shield :: Picture
shield = color white (Scale scaler scaler (text (show "U")))
          where scaler = 0.2


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
dead g@(GameState i t s sc _ _)= Pictures [score, shownew, showSave, showHome]
        where score        = showInfoToShow i sc
              shownew      = Translate 0 translate (textBox "New Game")
              showSave     = Translate 0 0 (textBox "Save Score")
              showHome     = Translate 0 (-translate) (textBox "Home")
              translate    = screenh * 0.5

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
maybeToScore s = Color white $ Scale scaler scaler $ Translate lengthString pos $ Text s
  where scaler = 0.2
        lengthString = -(screenw * 0.1 * fromIntegral (length s))
        pos = -(screenh * scaler)

