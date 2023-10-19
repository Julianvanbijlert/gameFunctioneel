module Controller where
  -- | This module defines how the state changes
  --   in response to time and user input

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState(InfoToShow b p xs) _)
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do let enem = moveAllEnemies xs
       return $ GameState (InfoToShow b p enem) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

moveAllEnemies :: [Enemy] -> [Enemy]
moveAllEnemies [] =  []
moveAllEnemies (e@(SpaceShip (Point(x, y)) (Vector(dx, dy))) : xs) =  SpaceShip (Point(x + dx, y + dy )) (Vector(dx, dy)) : moveAllEnemies xs
moveAllEnemies (e@(Rock (Point(x, y)) (Vector(dx, dy))) : xs) =  Rock (Point(x + dx, y + dy )) (Vector(dx, dy)) : moveAllEnemies xs
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState 
inputKey (EventKey (Char c) _ _ _) gstate@(GameState i e) 
  = -- If the user presses a character key, show that one
    gstate { infoToShow = handleInput c i }
inputKey _ gstate = gstate -- Otherwise keep the same

handleInput :: Char -> InfoToShow -> InfoToShow
handleInput 'w' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e) = InfoToShow b (Player (Point(x, y + dy)) (Vector(dx, dy))) e
handleInput 's' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e) = InfoToShow b (Player (Point(x, y - dy)) (Vector(dx, dy))) e
handleInput _ i = i