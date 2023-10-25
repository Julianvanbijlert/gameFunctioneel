{-# LANGUAGE MultiParamTypeClasses #-}

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
step secs gstate@(GameState(InfoToShow b p xs bs) _ s sc _)
  | elapsedTime gstate + secs > numberOfSecsBetweenActions
  = --nog random toevoegen
    return . spawnEnemyOrPowerUp 6 . checkState $ gstate{ elapsedTime = 0}


  | otherwise
  = -- Just update the elapsed time
     return . checkState $ gstate{ elapsedTime = elapsedTime gstate + secs }

checkState :: GameState -> GameState
checkState gstate@(GameState i _ Running sc _) = collideFunction . shootBulletEs . moveEverything $ gstate {score = sc + 1 }
checkState gstate@(GameState i t Paused sc _) = gstate
checkState gstate@(GameState i t GameOver sc _) = gstate

collideFunction :: GameState -> GameState
collideFunction gstate@(GameState(InfoToShow _ p xs bs) _ _ sc _) =
    if checkIfCollided p xs || checkIfCollided p bs
         then (.) checkDead removeHeart gstate{score = sc - 10}
         else gstate

moveEverything :: GameState -> GameState
moveEverything (GameState (InfoToShow b p xs bs) t state sc sg) = GameState (InfoToShow b p enem bul) t state sc sg
                                                                where enem = moveAllEnemies xs
                                                                      bul  = moveAllBullets bs
checkDead :: GameState -> GameState
checkDead gstate@(GameState i _ Running _ _)  | isDead i = gstate{state = GameOver}
                                             | otherwise = gstate
isDead :: InfoToShow -> Bool
isDead (InfoToShow b (Player k l []) xs bs) = True
isDead (InfoToShow b (Player k l xz) xs bs) = False

moveAllEnemies :: [Enemy] -> [Enemy]
moveAllEnemies [] =  []
moveAllEnemies (e@(SpaceShip (Point(x, y)) (Vector(dx, dy))) : xs) =  SpaceShip (Point (x + dx, y + dy )) (Vector (dx, dy)) : moveAllEnemies xs
moveAllEnemies (e@(Rock (Point(x, y)) (Vector(dx, dy))) : xs) =  Rock (Point (x + dx, y + dy )) (Vector (dx, dy)) : moveAllEnemies xs

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets [] = []
moveAllBullets ((EnemyBullet  (Point(x,y)) (Vector(dx, dy))) : xs) = EnemyBullet  (Point (x + dx, y + dy)) (Vector (dx, dy)) : moveAllBullets xs
moveAllBullets ((PlayerBullet (Point(x,y)) (Vector(dx, dy))) : xs)   = PlayerBullet (Point (x + dx, y + dy)) (Vector (dx, dy)) : moveAllBullets xs  --moet nog vervangen worden maar voor nu ff

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

class Collides s a where
  collides :: s -> a -> Bool
  checkIfCollided :: s -> [a] -> Bool
instance Collides Enemy Bullet where
  collides e (EnemyBullet _ _) = False
  collides (Rock (Point(x, y)) _ ) (PlayerBullet (Point(a,b)) _)= a>=x-17 &&a<=x+17&& b>=y-20 &&b>=y+20 -- | hardcoded en vierkant collision
  collides  (SpaceShip (Point (x, y)) _ ) (PlayerBullet (Point(a,b)) _) = a>=x-10 && a<=x+10 && b<=y-10 && b>= y+10 --hardcoded
  checkIfCollided e = any (collides e)
instance Collides Player Bullet where
  collides p (PlayerBullet _ _) = False
  collides (Player (Point(x,y)) _ _) (EnemyBullet(Point(a,b)) _) = a>=x && a<=x+30 && b>=y-10 &&b <=y+10 -- | hardcoded en vierkant collision
  checkIfCollided p = any (collides p)
instance Collides Player Enemy where
  collides (Player(Point(x,y)) _ _) e= collides e (PlayerBullet (Point (x,y-10)) (Vector (x,x))) || collides e (PlayerBullet (Point (x,y+10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y-10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y+10)) (Vector (x,x)))  -- again het is vierkant...
  checkIfCollided p = any (collides p)


destroyEnemy :: Enemy -> Enemy --redo
destroyEnemy = undefined

removeHeart :: GameState -> GameState
removeHeart (GameState (InfoToShow b (Player k l []) xs bs) m n o q) = GameState (InfoToShow b p xs bs) m n o q
                                                                        where p = Player k l []
removeHeart (GameState (InfoToShow b (Player k l [x]) xs bs) m n o q) = GameState (InfoToShow b p xs bs) m n o q
                                                                        where p = Player k l []
removeHeart (GameState (InfoToShow b (Player k l (y:ys)) xs bs) m n o q) = GameState (InfoToShow b p xs bs) m n o q
                                                                        where p = Player k l ys

endGame :: GameState -> GameState
endGame = undefined

pauseGame :: GameState -> GameState
pauseGame = undefined

spawnPowerup :: GameState -> GameState
spawnPowerup = undefined

spawnEnemyOrPowerUp :: Float -> GameState -> GameState
spawnEnemyOrPowerUp i g@(GameState (InfoToShow b p e h) k n m sg) | i > 5 = GameState (InfoToShow b p (fst (randomEnemy g) : e) h) k n m (snd (randomEnemy g))
                                                                | otherwise = g{infoToShow = InfoToShow b p e h} --nog powerup toevoegen

makeRandomCoordinate :: StdGen -> Float -> Float -> (Float, StdGen)
makeRandomCoordinate g0 x y = (a, g1)
    where
      (a,g1) =  randomR (x,y :: Float) g0

randomEnemy :: GameState-> (Enemy, StdGen)
randomEnemy g@(GameState (InfoToShow _ (Player(Point(x,y)) _ _) _ _) _ _ _ sg)  | (fst f) < 5  = (SpaceShip (Point p) (Vector (fst p - x, snd p - y)), snd f)
                                                                                | otherwise = (Rock (Point p) (Vector(fst p - x, snd p - y)), snd f)
                                                                                    where f = makeRandomCoordinate sg 0 100
                                                                                          p = (fst f, 350)


randomPowerup :: Powerup
randomPowerup = undefined

shootBulletEs :: GameState -> GameState
shootBulletEs g@(GameState i _ _ _ _) = g{infoToShow = shootBulletE 0 [] i}

--doet nog niets met random
shootBulletE :: Float -> [Enemy] -> InfoToShow -> InfoToShow
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(Rock p vec): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(SpaceShip (Point(xt, yt)) vec): es) bul) =
      shootBulletE random (e : le) (InfoToShow b (Player (Point (x,y)) v h) es (EnemyBullet (Point (xt - 20, yt) ) (Vector (-10, 0)) : bul))
shootBulletE random le i@(InfoToShow b (Player (Point(x,y)) v _) [] bul) = i{enemies = le}


--if spaceship hits a wall it will go back in the screen, if rock does this it breaks
hittWall :: Enemy -> Enemy
hittWall (SpaceShip p (Vector(dx, dy))) = SpaceShip p (Vector (dx, -dy))
hittWall rock = destroyEnemy rock

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState i e Running sc _)
  = gstate{state = Paused}
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState i e Paused sc _)
  = gstate{state = Running}
inputKey (EventKey (Char c) Down _  _) gstate@(GameState i e Running sc _)
  = -- If the user presses a character key, show that one
  gstate { infoToShow = handleInput c i }
inputKey (EventKey (SpecialKey k) Down _ _) gstate@(GameState i e Running sc _)
  = gstate {infoToShow = handleInputSpecial k i}

inputKey _ gstate@(GameState i e Paused sc _) = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

{-Dit is een functie die inputs handled, alleen moet er nog gefixt worden dat ingedrukt houden 
meerdere interacties doet en dat het niet dubbel aangeroepen is als het omhoog gaat en losgelaten wordt-}
handleInput :: Char -> InfoToShow -> InfoToShow
handleInput 'w' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y + dy)) (Vector (dx, dy)) h) e bul
handleInput 's' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y - dy)) (Vector (dx, dy)) h) e bul
handleInput 'f' (InfoToShow b p@(Player (Point(x, y)) _ _) e bul) = InfoToShow b p e (PlayerBullet (Point (x + 40, y)) (Vector (5, 0)) : bul)
handleInput _ i = i

handleInputSpecial :: SpecialKey -> InfoToShow -> InfoToShow
handleInputSpecial KeyUp    (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y + dy)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeyDown  (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y - dy)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeySpace (InfoToShow b p@(Player (Point(x, y)) _ _) e bul) = InfoToShow b p e (PlayerBullet (Point (x + 40, y)) (Vector (5, 0)) : bul)
handleInputSpecial _ i = i