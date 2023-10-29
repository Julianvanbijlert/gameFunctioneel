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
      InfoToShow(ShowAChar, ShowNothing, InfoToShow, ShowANumber),
      GameState(GameState, infoToShow, state),
      screenw,
      screenh, )


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@(GameState i t s sc _) = case state gstate of
  Running -> case infoToShow gstate of
            ShowNothing   -> blank
            InfoToShow b p xs bs -> Pictures [showBorder b, showPlayer p, showListEnemies xs, showBullets bs, showScore sc, showLives p]
            ShowANumber n -> color green (text (show n))
            ShowAChar   c -> color green (text [c])
  Paused -> pause
  GameOver -> gameOver


showPlayer :: Player -> Picture
showPlayer (Player (Point(x, y)) _ _) = color green (Polygon [(x, y-10), (x, y +10), (x+30, y)] ) --triangle

showListEnemies :: [Enemy] -> Picture
showListEnemies [] = blank
showListEnemies (x: xs) = Pictures[showEnemy x, showListEnemies xs]

showEnemy :: Enemy -> Picture
showEnemy  (Rock (Point(x, y)) _ _) = color white (Polygon [(x, y+20), (x+17, y+10), (x+17, y-10), (x, y-20), (x-17, y-10), (x-17, y+10)])
showEnemy  (SpaceShip (Point (x, y)) _ _) = color red (Polygon [(x-10, y-10),(x+10, y+10), (x-10, y+10), (x+10, y-10)]) --square
showEnemy  (Jet (Point (x, y)) _ _) = color blue (Polygon [(x-7, y-7),(x+7, y+7), (x-7, y+7), (x+7, y-7)]) --square
showEnemy  (MotherShip (Point (x, y)) _ _) = color green (Polygon [(x-20, y-20),(x+20, y+20), (x-20, y+20), (x+20, y-20)]) --square


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
                      showControls  = Translate 0 (-screenh * 0.5)(textBox "Controls")

textBox :: String -> Picture
textBox s = Pictures [box, text]
          where box = Color white $ rectangleWire (screenw * 0.5) (screenh * 0.2)
                text = Color white $ Scale 0.2 0.2 $ Translate (-(screenw * 0.1 * fromIntegral (length s))) (-(screenh * 0.2)) $ Text s

stateAction :: State -> Picture --niet goed
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined