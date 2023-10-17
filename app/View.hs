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
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowPlayer (Player (Point(x, y)) p) -> color green (Polygon [(x, y-10), (x, y +10), (x+30, y)] )--[(0, 0), (30, 10), (0, 20)])
  showRock -> color white (Polygon [(0, 50), (43.3, 25), (43.3, -25), (0, -50), (-43.3, -25), (-43.3, 25)])


stateAction :: State -> Picture --niet goed
stateAction Running = undefined
stateAction Paused = undefined
stateAction GameOver = undefined