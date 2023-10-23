module Controller where
  -- | This module defines how the state changes
  --   in response to time and user input

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Color

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState(InfoToShow b p xs bs) _)
  | elapsedTime gstate + secs > numberOfSecsBetweenActions
  = 
    return $ moveEverything gstate { elapsedTime = 0 }
    
  | otherwise
  = -- Just update the elapsed time
     return $ moveEverything gstate { elapsedTime = elapsedTime gstate + secs }
    


moveEverything :: GameState -> GameState
moveEverything (GameState(InfoToShow b p xs bs) t) =  do let enem = moveAllEnemies xs
                                                         let bul  = moveAllBullets bs
                                                         GameState (InfoToShow b p enem bul) t

moveAllEnemies :: [Enemy] -> [Enemy]
moveAllEnemies [] =  []
moveAllEnemies (e@(SpaceShip (Point(x, y)) (Vector(dx, dy))) : xs) =  SpaceShip (Point(x + dx, y + dy )) (Vector(dx, dy)) : moveAllEnemies xs
moveAllEnemies (e@(Rock (Point(x, y)) (Vector(dx, dy))) : xs) =  Rock (Point(x + dx, y + dy )) (Vector(dx, dy)) : moveAllEnemies xs

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets [] = []
moveAllBullets ((EnemyBullet  (Point(x,y)) (Vector(dx, dy))) : xs) = EnemyBullet  (Point(x + dx, y + dy)) (Vector(dx, dy)) : moveAllBullets xs
moveAllBullets ((PlayerBullet (Point(x,y)) (Vector(dx, dy))) : xs)   = PlayerBullet (Point(x + dx, y + dy)) (Vector(dx, dy)) : moveAllBullets xs  --moet nog vervangen worden maar voor nu ff

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

collides :: Player -> Enemy -> Bool
collides  = undefined

destroyEnemy :: Enemy -> Enemy --redo
destroyEnemy = undefined

removeHeart :: GameState -> GameState
removeHeart = undefined 

endGame :: GameState -> GameState
endGame = undefined

pauseGame :: GameState -> GameState
pauseGame = undefined

spawnPowerup :: GameState -> GameState
spawnPowerup = undefined

shootBulletP :: GameState -> GameState
shootBulletP = undefined

shootBulletE :: GameState -> GameState
shootBulletE = undefined

--if spaceship hits a wall it will go back in the screen, if rock does this it breaks
hittWall :: Enemy -> Enemy
hittWall (SpaceShip p (Vector(dx, dy))) = SpaceShip p (Vector(dx, -dy))
hittWall rock = destroyEnemy rock

inputKey :: Event -> GameState -> GameState 
inputKey (EventKey (Char c) Down _  _) gstate@(GameState i e) 
  = -- If the user presses a character key, show that one
    gstate { infoToShow = handleInput c i }
       
inputKey (EventKey (SpecialKey k) Down _ _) gstate@(GameState i e) 
  = 
    gstate {infoToShow = handleInputSpecial k i}
inputKey _ gstate = gstate -- Otherwise keep the same

{-Dit is een functie die inputs handled, alleen moet er nog gefixt worden dat ingedrukt houden 
meerdere interacties doet en dat het niet dubbel aangeroepen is als het omhoog gaat en losgelaten wordt-}
handleInput :: Char -> InfoToShow -> InfoToShow
handleInput 'w' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e bul) = InfoToShow b (Player (Point(x, y + dy)) (Vector(dx, dy))) e bul
handleInput 's' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e bul) = InfoToShow b (Player (Point(x, y - dy)) (Vector(dx, dy))) e bul
handleInput 'f' (InfoToShow b p@(Player (Point(x, y)) _ ) e bul) = InfoToShow b p e (PlayerBullet (Point(x + 40, y)) (Vector(5, 0)) : bul)
handleInput _ i = i

handleInputSpecial :: SpecialKey -> InfoToShow -> InfoToShow
handleInputSpecial KeyUp    (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e bul) = InfoToShow b (Player (Point(x, y + dy)) (Vector(dx, dy))) e bul
handleInputSpecial KeyDown  (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy))) e bul) = InfoToShow b (Player (Point(x, y - dy)) (Vector(dx, dy))) e bul
handleInputSpecial KeySpace (InfoToShow b p@(Player (Point(x, y)) _ ) e bul) = InfoToShow b p e (PlayerBullet (Point(x + 40, y)) (Vector(5, 0)) : bul)
handleInputSpecial _ i = i