module View where
  -- | This module defines how to turn
  --   the game state into a picture

import Graphics.Gloss
import Model
    ( Heart(..),
      Point(..),
      Border(..),
      Bullet(..),
      Enemy(..),
      Player(..),
      State(..),
      InfoToShow(ShowAChar, ShowNothing, InfoToShow, ShowANumber, ShowHighScores),
      GameState(GameState, infoToShow, state),
      screenw,
      screenh, )

import Data.List
import Data.Ord (comparing)



view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@(GameState i t s sc hs _) = case state gstate of
  Running -> case infoToShow gstate of
            ShowNothing   -> blank
            InfoToShow b p xs bs -> showInfoToShow i sc
            ShowANumber n -> color green (text (show n))
            ShowAChar   c -> color green (text [c])
  Paused -> pause
  GameOver -> case infoToShow gstate of
                  InfoToShow b p xs bs -> gameOver
                  ShowHighScores -> showHighScores $ orderList hs 
  Dead -> dead gstate

showInfoToShow :: InfoToShow -> Int -> Picture
showInfoToShow (InfoToShow b p xs bs) sc = Pictures [showBorder b, showPlayer p, showListEnemies xs, showBullets bs, showScore sc, showLives p]

showPlayer :: Player -> Picture
showPlayer (Player (Point(x, y)) _ _) = color green (Polygon [(x, y-10), (x, y +10), (x+30, y)] ) --triangle

showListEnemies :: [Enemy] -> Picture
showListEnemies [] = blank
showListEnemies e = Pictures $ map showEnemy e

showEnemy :: Enemy -> Picture
showEnemy e@(Rock _ _ [] _) = showExplosion e
showEnemy e@(SpaceShip _ _ [] _) = showExplosion e
showEnemy e@(Jet _ _ [] _) = showExplosion e
showEnemy e@(MotherShip _ _ [] _) = showExplosion e
showEnemy  (Rock (Point(x, y)) _ _ _) = color white (Polygon [(x, y+20), (x+17, y+10), (x+17, y-10), (x, y-20), (x-17, y-10), (x-17, y+10)])
showEnemy  (SpaceShip (Point (x, y)) _ _ _) = color red (Polygon [(x-10, y-10),(x+10, y+10), (x-10, y+10), (x+10, y-10)]) --square
showEnemy  (Jet (Point (x, y)) _ _ _) = color blue (Polygon [(x-7, y-7),(x+7, y+7), (x-7, y+7), (x+7, y-7)]) --square
showEnemy  (MotherShip (Point (x, y)) _ _ _) = color green (Polygon [(x-20, y-20),(x+20, y+20), (x-20, y+20), (x+20, y-20)]) --square

showExplosion :: Enemy -> Picture 
showExplosion  (Rock (Point(x, y)) _ _ h) = Pictures[color orange (Polygon [(x-9-c, y-17-c), (x-9-c, y+17+c), (x+17+c, y)]),color orange (Polygon [(x+9+c, y-17-c), (x+9+c, y+17+c), (x-17-c, y)])]
            where c = h/10
showExplosion  (SpaceShip (Point (x, y)) _ _ h) = Pictures[color orange (Polygon [(x-4-c, y-10-c),(x-4-c, y+10+c), (x+10+c, y)]), color orange (Polygon [(x+4+c, y-10-c),(x+4+c, y+10+c), (x-10-c, y)])] --square
            where c = h/10
showExplosion  (Jet (Point (x, y)) _ _ h) = Pictures[color orange (Polygon [(x-2-c, y-7-c),(x-2-c, y+7+c), (x+7+c, y)]), color orange (Polygon [(x+2+c, y-7-c),(x+2+c, y+7+c), (x-7-c, y)]) ]--square
            where c = h/10
showExplosion  (MotherShip (Point (x, y)) _ _ h) = Pictures[color orange (Polygon [(x-9-c, y-20-c),(x-9-c, y+20+c), (x+20+c, y)]),color orange (Polygon [(x+9+c, y-20-c),(x+9+c, y+20+c), (x-20-c, y)])] --square
            where c = h/10

showBullets :: [Bullet] -> Picture
showBullets [] = blank
showBullets (x: xs) = Pictures[showBullet x, showBullets xs]

showBullet :: Bullet -> Picture
showBullet (EnemyBullet (Point(x, y)) _) = color yellow (Polygon[(x - 2, y -1),(x + 2, y - 1), (x + 2, y + 1), (x - 2, y + 1)])
showBullet (PlayerBullet (Point(x, y)) _) = color white (Polygon[(x - 2, y -1),(x + 2, y - 1), (x + 2, y + 1), (x - 2, y + 1)])

showBorder :: Border -> Picture
showBorder (Border ytop ybot) = pictures[ color green (Polygon [(sw, sh), (sw, ytop), (-sw , ytop), (-sw, sh) ]),
                                          color green (Polygon [(sw, -sh), (sw, ybot), (-sw , ybot), (-sw, -sh) ]) ]
                                            where sw = Model.screenw  --helft van screenwidth
                                                  sh = Model.screenh  --helft van screenheight
showScore :: Int -> Picture
showScore i = color white (Translate (-50) (Model.screenh - 50) (Scale 0.3 0.3 (text (show i))))

--we made this player instead of putting it in the game in case we wanted to add multiplayer
showLives :: Player -> Picture
showLives (Player (Point(x, y)) _ h) = showLive (Point(0, 0)) (reverse h)

showLive :: Model.Point -> [Heart] -> Picture
showLive _ [] = blank
showLive p@(Point (x, y)) (Heart : xs) = Pictures[drawHeart p, showLive (Point(x + 50, y)) xs]
showLive p@(Point (x, y)) (Shield : xs) = Pictures[drawShield p, showLive (Point(x + 50, y)) xs]

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
pause = Pictures[showContinue, showSave, showExit]
                where showContinue = Translate 0 (screenh * 0.5) (textBox "Continue")
                      showSave     = textBox "Save"
                      showExit     = Translate 0 (-screenh * 0.5) (textBox "Exit")

gameOver :: Picture
gameOver = Pictures[showStart, showHighScore, showControls]
                where showStart     = Translate 0 (screenh * 0.5) (textBox "Start game")
                      showHighScore = textBox "High Scores"
                      showControls  = Translate 0 (-screenh * 0.5)(textBox "Load game")

textBox :: String -> Picture
textBox s = Pictures [box, text]
          where box = Color white $ rectangleWire (screenw * 0.5) (screenh * 0.2)
                text = Color white $ Scale 0.2 0.2 $ Translate (-(screenw * 0.1 * fromIntegral (length s))) (-(screenh * 0.2)) $ Text s

dead :: GameState -> Picture
dead g@(GameState i t s sc _ _)= Pictures[score, shownew, showSave, showHome]
        where score        = showInfoToShow i sc--showScore sc
              shownew      = Translate 0 (screenh * 0.5) (textBox "New Game")
              showSave     = textBox "Save score"
              showHome     = Translate 0 (-screenh * 0.5) (textBox "Home")

stateAction :: State -> Picture --niet goed
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined
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
showHighScores (f: s: t: fo: fi: _) = Pictures[ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, scoreToPic (-0.1) fo, scoreToPic (-0.3) fi, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: t: fo:  _)    = Pictures[ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, scoreToPic (-0.1) fo, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: t: _)         = Pictures[ scoreToPic 0.5 f, scoreToPic 0.3 s, scoreToPic 0.1 t, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: s: _)            = Pictures[ scoreToPic 0.5 f, scoreToPic 0.3 s, Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores (f: _)               = Pictures[ scoreToPic 0.5 f,  Translate 0 (-screenh * 0.6) (textBox "Home")]
showHighScores []                   = Translate 0 (-screenh * 0.6) (textBox "Home")

scoreToPic :: Float -> String -> Picture
scoreToPic y s = Translate 0 (screenh * y)  (maybeToScore s)

maybeToScore :: String -> Picture
--maybeToScore Nothing = blank
maybeToScore s = Color white $ Scale 0.2 0.2 $ Translate (-(screenw * 0.1 * fromIntegral (length s))) (-(screenh * 0.2)) $ Text s

orderList :: [String] -> [String]
orderList = sortBy compareTwo

compareTwo :: String -> String -> Ordering
compareTwo a b = compare (read b :: Int) (read a :: Int) 