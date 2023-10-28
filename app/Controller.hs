{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Controller where
  -- | This module defines how the state changes
  --   in response to time and user input

import Model
    ( numberOfSecsBetweenActions,
      Powerup,
      Vector(Vector),
      Point(Point),
      State(Paused, GameOver, Running),
      Bullet(..),
      Enemy(..),
      Player(..),
      InfoToShow(InfoToShow, enemies),
      GameState(GameState, elapsedTime, score, state, infoToShow), Heart (Heart),Border (Border), screenw, screenh )



import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Color

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState i h s sc sg)
    | elapsedTime gstate + secs > numberOfSecsBetweenActions = --nog random toevoegen
      return . spawnEnemyOrPowerUp (fst j) .  shootBulletEs . checkState $ (GameState i h s sc (snd j)){ elapsedTime = 0}
    | otherwise = -- Just update the elapsed time
      return . checkState $ gstate{ elapsedTime = elapsedTime gstate + secs }
  where
    j = makeRandomCoordinate sg 0 80



checkState :: GameState -> GameState
checkState gstate@(GameState i _ Running sc _) = collideFunction . moveEverything $ gstate {score = sc + 1 }
checkState gstate@(GameState i t Paused sc _) = gstate
checkState gstate@(GameState i t GameOver sc _) = gstate

collideFunction :: GameState -> GameState
collideFunction gstate@(GameState(InfoToShow o p e b) _ _ _ _) =
   checkDead gstate{infoToShow = InfoToShow o p2 e3 b2}
    where p0 = collideFunctionBoard p o
          (p1, e1) = collideFunctionPlayer p0 e
          (p2, b1) = collideFunctionPlayer p1 b
          (e2, b2) = collideFunctionEnemy e1 b1
          e3 = filter(not.(`collides` o)) e2
          e4 = removeEnemies e3


collideFunctionPlayer :: (Collides s a, Remove s) => s -> [a] -> (s, [a])
collideFunctionPlayer p [] = (p, [])
collideFunctionPlayer p (x:xs) | collides p x = collideFunctionPlayer (removeHeart p) xs
                               | otherwise = let (q, ys) = collideFunctionPlayer p xs in (q, x:ys)

collideFunctionEnemy :: (Collides s a, Remove s) => [s] -> [a] -> ([s], [a])
collideFunctionEnemy e [] = (e, [])
collideFunctionEnemy [] b = ([], b)
collideFunctionEnemy (e:es) b = let (ys, zs) = collideFunctionEnemy es b1 in (e1:ys, zs)
      where (e1, b1) = collideFunctionPlayer e b

collideFunctionBoard :: (Collides s a, Remove s) => s -> a -> s
collideFunctionBoard p b | collides p b = destroy p
                         | otherwise = p

moveEverything :: GameState -> GameState
moveEverything (GameState (InfoToShow b p xs bs) t state sc sg) = GameState (InfoToShow b p enem bul) t state sc sg
                                                                where enem = moveAllEnemies xs
                                                                      bul  = moveAllBullets bs
checkDead :: GameState -> GameState
checkDead gstate@(GameState (InfoToShow _ p _ _) _ Running _ _)  | isDead p = gstate{state = GameOver}
                                                                | otherwise = gstate


moveAllEnemies :: [Enemy] -> [Enemy]
moveAllEnemies [] =  []
moveAllEnemies (e@(SpaceShip (Point(x, y)) (Vector(dx, dy)) h) : xs) =  SpaceShip (Point (x + dx, y + dy )) (Vector (dx, dy)) h : moveAllEnemies xs
moveAllEnemies (e@(Rock (Point(x, y)) (Vector(dx, dy)) h) : xs) =  Rock (Point (x + dx, y + dy )) (Vector (dx, dy)) h : moveAllEnemies xs

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets [] = []
moveAllBullets ((EnemyBullet  (Point(x,y)) (Vector(dx, dy))) : xs) = EnemyBullet  (Point (x + dx, y + dy)) (Vector (dx, dy)) : moveAllBullets xs
moveAllBullets ((PlayerBullet (Point(x,y)) (Vector(dx, dy))) : xs)   = PlayerBullet (Point (x + dx, y + dy)) (Vector (dx, dy)) : moveAllBullets xs  --moet nog vervangen worden maar voor nu ff

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

class Collides s a where
  collides :: s -> a -> Bool
instance Collides Enemy Bullet where
  collides e (EnemyBullet _ _) = False
  collides (Rock (Point(x, y)) _ _) (PlayerBullet (Point(a,b)) _)= a>=x-17 &&a<=x+17&& b>=y-20 &&b<=y+20 -- | hardcoded en vierkant collision
  collides  (SpaceShip (Point (x, y)) _ _) (PlayerBullet (Point(a,b)) _) = a>=x-10 && a<=x+10 && b>=y-10 && b<= y+10 --hardcoded
instance Collides Player Bullet where
  collides p (PlayerBullet _ _) = False
  collides (Player (Point(x,y)) _ _) (EnemyBullet(Point(a,b)) _) = a>=x && a<=x+30 && b>=y-10 &&b <=y+10 -- | hardcoded en vierkant collision
instance Collides Player Enemy where
  collides (Player(Point(x,y)) _ _) e= collides e (PlayerBullet (Point (x,y-10)) (Vector (x,x))) || collides e (PlayerBullet (Point (x,y+10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y-10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y+10)) (Vector (x,x)))  -- again het is vierkant...
instance Collides Player Border where
  collides (Player (Point(x,y)) _ _) (Border a b) = y+10>=a || y-10<=b
instance Collides Enemy Border where
  collides (Rock (Point(x, y)) _ _) (Border a b) = x-17>a ||x+17<b|| y+20<b ||y-20>a
  collides  (SpaceShip (Point (x, y)) _ _) (Border a b) = x-10>a ||x+10<b|| y+10<b ||y-10>a

class Remove p where
  removeHeart :: p -> p
  isDead :: p -> Bool
  destroy :: p -> p
instance Remove Player where
  removeHeart (Player p v [x]) = Player p v []
  removeHeart (Player p v (x:xs)) = Player p v xs
  isDead (Player p v []) = True
  isDead (Player p v a) = False
  destroy (Player p v x) = Player p v []
instance Remove Enemy where
  removeHeart (Rock p v [x]) = Rock p v []
  removeHeart (SpaceShip p v [x]) = SpaceShip p v []
  removeHeart (Rock p v (x:xs)) = Rock p v xs
  removeHeart (SpaceShip p v (x:xs)) = SpaceShip p v xs
  isDead (Rock p v []) = True
  isDead (SpaceShip p v []) = True
  isDead (Rock p v a) = False
  isDead (SpaceShip p v a) = False
  destroy (Rock p v x) = Rock p v []
  destroy (SpaceShip p v x) = SpaceShip p v []


removeEnemies :: [Enemy] -> [Enemy]
removeEnemies = filter (not.isDead)

endGame :: GameState -> GameState
endGame = undefined

pauseGame :: GameState -> GameState
pauseGame = undefined

spawnPowerup :: GameState -> GameState
spawnPowerup = undefined

spawnEnemyOrPowerUp :: Float -> GameState -> GameState

spawnEnemyOrPowerUp i g@(GameState (InfoToShow b p e h) k Running m sg) | i > 79 = GameState (InfoToShow b p (fst (randomEnemy g) : e) h) k Running m (snd (randomEnemy g))
                                                                | otherwise = g{infoToShow = InfoToShow b p e h} --nog powerup toevoegen
spawnEnemyOrPowerUp i g = g

makeRandomCoordinate :: StdGen -> Float -> Float -> (Float, StdGen)
makeRandomCoordinate g0 x y = (a, g1)
    where
      (a,g1) =  randomR (x,y :: Float) g0

normalize :: Model.Vector -> Model.Vector
normalize (Vector (x,y))= Vector (x / p, y / p)
          where p = sqrt (x*x + y*y)

randomEnemy :: GameState-> (Enemy, StdGen)
randomEnemy g@(GameState (InfoToShow _ (Player(Point(x,y)) _ _) _ _) _ _ _ sg)  | i >= 5  = (SpaceShip (Point p) v [Heart, Heart, Heart], s1)
                                                                                | otherwise = (Rock (Point p) v [Heart], s1)
                                                                                    where (y1, s) = makeRandomCoordinate sg (-screenh + 10) (screenh - 10)
                                                                                          (i, s1) = makeRandomCoordinate s 0 10
                                                                                          p@(a,b) = (screenw,y1)
                                                                                          v = normalize (Vector (x- a , y-b))



randomPowerup :: Powerup
randomPowerup = undefined

shootBulletEs :: GameState -> GameState
shootBulletEs g@(GameState i _ _ _ _) = g{infoToShow = shootBulletE 0 [] i}

--doet nog niets met random
shootBulletE :: Float -> [Enemy] -> InfoToShow -> InfoToShow
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(Rock p vec l): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(SpaceShip (Point(xt, yt)) vec l): es) bul) =
      shootBulletE random (e : le) (InfoToShow b (Player (Point (x,y)) v h) es (EnemyBullet (Point (xt - 20, yt) ) (Vector (-10, 0)) : bul))
shootBulletE random le i@(InfoToShow b (Player (Point(x,y)) v _) [] bul) = i{enemies = le}


--if spaceship hits a wall it will go back in the screen, if rock does this it breaks
hittWall :: Enemy -> Enemy
hittWall (SpaceShip p (Vector(dx, dy)) h) = SpaceShip p (Vector (dx, -dy)) h
hittWall rock = destroy rock

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
handleInput 'a' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x - dx, y)) (Vector (dx, dy)) h) e bul
handleInput 'd' (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x + dx, y)) (Vector (dx, dy)) h) e bul
handleInput 'f' (InfoToShow b p@(Player (Point(x, y)) _ _) e bul) = InfoToShow b p e (PlayerBullet (Point (x + 40, y)) (Vector (5, 0)) : bul)
handleInput _ i = i

handleInputSpecial :: SpecialKey -> InfoToShow -> InfoToShow
handleInputSpecial KeyUp    (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y + dy)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeyDown  (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x, y - dy)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeyLeft (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x - dx, y)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeyRight (InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = InfoToShow b (Player (Point (x + dx, y)) (Vector (dx, dy)) h) e bul
handleInputSpecial KeySpace (InfoToShow b p@(Player (Point(x, y)) _ _) e bul) = InfoToShow b p e (PlayerBullet (Point (x + 40, y)) (Vector (5, 0)) : bul)
handleInputSpecial _ i = i