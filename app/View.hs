module View where
  -- | This module defines how to turn
  --   the game state into a picture
  
import Graphics.Gloss
import Model


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  InfoToShow b p xs -> Pictures [showBorder b, showPlayer p, showListEnemies xs]
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])


showPlayer :: Player -> Picture
showPlayer (Player (Point(x, y)) _) = color green (Polygon [(x, y-10), (x, y +10), (x+30, y)] )--triangle

showListEnemies :: [Enemy] -> Picture
showListEnemies [] = blank
showListEnemies (x: xs) = Pictures[showEnemy x, showListEnemies xs]

showEnemy :: Enemy -> Picture
showEnemy  (Rock (Point(x, y)) _ ) = color white (Polygon [(x, y+20), (x+17, y+10), (x+17, y-10), (x, y-20), (x-17, y-10), (x-17, y+10)]) 
showEnemy  (SpaceShip (Point (x, y)) _ ) = color red (Polygon [(x-10, y-10),(x+10, y+10), (x-10, y+10), (x+10, y-10)]) --square

showBorder :: Border -> Picture
showBorder (Border ytop ybot) = pictures[ color green (Polygon [(sw, sh), (sw, ytop), (-sw , ytop), (-sw, sh) ]),
                                          color green (Polygon [(sw, -sh), (sw, ybot), (-sw , ybot), (-sw, -sh) ]) ]
                                            where sw = 350  --helft van screenwidth
                                                  sh = 350  --helft van screenheight



stateAction :: State -> Picture --niet goed
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined