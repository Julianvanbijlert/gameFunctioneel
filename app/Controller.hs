{-# LANGUAGE MultiParamTypeClasses #-}
module Controller where
  -- | This module defines how the state changes
  --   in response to time and user input

import Model
    ( numberOfSecsBetweenActions,
      Powerup,
      Vector(Vector),
      Point(Point),
      State(Paused, GameOver, Running, Dead),
      Bullet(..),
      Enemy(..),
      Player(..),
      InfoToShow(InfoToShow, enemies),
      GameState(GameState, elapsedTime, score, state, infoToShow), Heart (Heart),Border (Border), screenw, screenh,
      initialState
    )



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
    j = makeRandomCoordinate sg 0 60



checkState :: GameState -> GameState
checkState gstate@(GameState i _ Running sc _) = collideFunction . moveEverything $ gstate {score = sc + 1 }
checkState gstate@(GameState i t Paused sc _) = gstate
checkState gstate@(GameState i t GameOver sc _) = gstate
checkState gstate@(GameState i t Dead sc _) = gstate

collideFunction :: GameState -> GameState
collideFunction gstate@(GameState(InfoToShow o p e b) t s sc sg) =
   checkDead gstate{infoToShow = InfoToShow o p2 e4 b2}
    where p0 = collideFunctionBoard p o
          (p1, e1, sc1) = collideFunctions p0 e sc
          (p2, b1, sc2) = collideFunctions p1 b sc1
          (e2, b2, sc3) = collideFunctionEnemy e1 b1 sc2
          e3 = filter (not.(`collides` o)) e2
          e4 = removeEnemies e3

collideFunctionEnemy :: (Collides s a, Remove s, Num score) => [s] -> [a] -> score -> ([s], [a], score)
collideFunctionEnemy e [] sc = (e, [], sc)
collideFunctionEnemy [] b sc = ([], b, sc)
collideFunctionEnemy (e:es) b sc = let (ys, zs, sc0) = collideFunctionEnemy es b1 s in (e1:ys, zs, sc0)
      where (e1, b1, s) = collideFunctions e b sc

collideFunctionBoard :: (Collides s a, Remove s) => s -> a -> s
collideFunctionBoard p b | collides p b = destroy p
                         | otherwise = p

moveEverything :: GameState -> GameState
moveEverything (GameState (InfoToShow b p xs bs) t state sc sg) = GameState (InfoToShow b p enem bul) t state sc sg
                                                                where enem = moveAllEnemies xs
                                                                      bul  = moveAllBullets bs
checkDead :: GameState -> GameState
checkDead gstate@(GameState (InfoToShow _ p _ _) _ Running _ _)  | isDead p = gstate{state = Dead}
                                                                 | otherwise = gstate


moveAllEnemies :: [Enemy] -> [Enemy]
moveAllEnemies [] =  []
moveAllEnemies (e@(Rock _ _ [] _) :es) = e : moveAllEnemies es
moveAllEnemies (e@(SpaceShip _ _ [] _):es) = e : moveAllEnemies es
moveAllEnemies (e@(Jet _ _ [] _):es) = e : moveAllEnemies es
moveAllEnemies (e@(MotherShip _ _ [] _):es) =  e : moveAllEnemies es
moveAllEnemies (e@(SpaceShip (Point(x, y)) (Vector(dx, dy)) h s) : xs) =  SpaceShip (Point (x + dx, y + dy )) (Vector (dx, dy)) h s : moveAllEnemies xs
moveAllEnemies (e@(Rock (Point(x, y)) (Vector(dx, dy)) h s) : xs) =  Rock (Point (x + dx, y + dy )) (Vector (dx, dy)) h s : moveAllEnemies xs
moveAllEnemies (e@(Jet (Point(x, y)) (Vector(dx, dy)) h s) : xs) =  Jet (Point (x + dx, y + dy )) (Vector (dx, dy)) h s : moveAllEnemies xs
moveAllEnemies (e@(MotherShip (Point(x, y)) (Vector(dx, dy)) h s) : xs) =  MotherShip (Point (x + dx, y + dy )) (Vector (dx, dy)) h s : moveAllEnemies xs

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets [] = []
moveAllBullets ((EnemyBullet  (Point(x,y)) (Vector(dx, dy))) : xs) = EnemyBullet  (Point (x + 2*dx, y + 2*dy)) (Vector (dx, dy)) : moveAllBullets xs
moveAllBullets ((PlayerBullet (Point(x,y)) (Vector(dx, dy))) : xs)   = PlayerBullet (Point (x + dx, y + dy)) (Vector (dx, dy)) : moveAllBullets xs  --moet nog vervangen worden maar voor nu ff

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

class Collides s a where
  collides :: s -> a -> Bool
  collideFunctions :: Num score => s -> [a] -> score -> (s, [a], score)
instance Collides Enemy Bullet where
  collides e (EnemyBullet _ _) = False
  collides (Rock (Point(x, y)) _ _ _) (PlayerBullet (Point(a,b)) _)= a>=x-17 &&a<=x+17&& b>=y-20 &&b<=y+20 -- | hardcoded en vierkant collision
  collides  (SpaceShip (Point (x, y)) _ _ _) (PlayerBullet (Point(a,b)) _) = a>=x-10 && a<=x+10 && b>=y-10 && b<= y+10 --hardcoded
  collides  (Jet (Point (x, y)) _ _ _) (PlayerBullet (Point(a,b)) _) = a>=x-7 && a<=x+7 && b>=y-7 && b<= y+7 --hardcoded
  collides  (MotherShip (Point (x, y)) _ _ _) (PlayerBullet (Point(a,b)) _) = a>=x-20 && a<=x+20 && b>=y-20 && b<= y+20
  collideFunctions e [] sc = (e, [], sc)
  collideFunctions r@(Rock {}) (x:xs) sc| collides r x = collideFunctions (removeHeart r) xs (sc + 100)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(SpaceShip {}) (x:xs) sc| collides r x = collideFunctions (removeHeart r) xs (sc + 200)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(MotherShip {}) (x:xs) sc| collides r x = collideFunctions (removeHeart r) xs (sc + 1000)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(Jet {}) (x:xs) sc| collides r x = collideFunctions (removeHeart r) xs (sc + 500)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
instance Collides Player Bullet where
  collides p (PlayerBullet _ _) = False
  collides (Player (Point(x,y)) _ _) (EnemyBullet(Point(a,b)) _) = a>=x && a<=x+30 && b>=y-10 &&b <=y+10 -- | hardcoded en vierkant collision
  collideFunctions p [] sc = (p, [], sc)
  collideFunctions p (x:xs) sc| collides p x = collideFunctions (removeHeart p) xs (sc-100)
                                 | otherwise = let (q, ys, s) = collideFunctions p xs sc in (q, x:ys, s)
instance Collides Player Enemy where
  collides (Player(Point(x,y)) _ _) e = collides e (PlayerBullet (Point (x,y-10)) (Vector (x,x))) || collides e (PlayerBullet (Point (x,y+10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y-10)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+30,y+10)) (Vector (x,x)))  -- again het is vierkant...
  collideFunctions p [] sc = (p, [], sc)
  collideFunctions p (x:xs) sc | collides p x = let (q, ys, s) = collideFunctions (removeHeart p) xs (sc-200) in (q, destroy x:ys, s)
                               | otherwise = let (q, ys, s) = collideFunctions p xs sc in (q, x:ys, s)
instance Collides Player Border where
  collides (Player (Point(x,y)) _ _) (Border a b) = y+10>=a || y-10<=b || x<=(-screenw) || x+30>=screenw
  collideFunctions p x sc = (p, x, sc)
instance Collides Enemy Border where
  collides (Rock (Point(x, y)) _ _ _) (Border a b) = x-17>screenw ||x+17<(-screenw)|| y+20<(-screenh) ||y-20>screenh
  collides  (SpaceShip (Point (x, y)) _ _ _) (Border a b) = x-10>screenw ||x+10<(-screenw)|| y+10<(-screenh) ||y-10>screenh
  collides  (Jet (Point (x, y)) _ _ _) (Border a b) = x-7>screenw ||x+7<(-screenw)|| y+7<(-screenh) ||y-7>screenh
  collides  (MotherShip (Point (x, y)) _ _ _) (Border a b) = x-20>screenw ||x+20<(-screenw)|| y+20<(-screenh) ||y-20>screenh
  collideFunctions p x sc = (p, x, sc)

class Remove p where
  removeHeart :: p -> p
  isDead :: p -> Bool
  destroy :: p -> p
instance Remove Player where
  removeHeart (Player p v []) = Player p v []
  removeHeart (Player p v [x]) = Player p v []
  removeHeart (Player p v (x:xs)) = Player p v xs
  isDead (Player p v []) = True
  isDead (Player p v a) = False
  destroy (Player p v x) = Player p v []
instance Remove Enemy where
  removeHeart (Rock p v [] c) = Rock p v [] c
  removeHeart (SpaceShip p v [] c) = SpaceShip p v [] c
  removeHeart (Jet p v [] c) = Jet p v [] c
  removeHeart (MotherShip p v [] c) = MotherShip p v [] c
  removeHeart (Rock p v [x] c) = Rock p v [] c
  removeHeart (SpaceShip p v [x] c) = SpaceShip p v [] c
  removeHeart (Jet p v [x] c) = Jet p v [] c
  removeHeart (MotherShip p v [x] c) = MotherShip p v [] c
  removeHeart (Rock p v (x:xs) c) = Rock p v xs c
  removeHeart (SpaceShip p v (x:xs) c) = SpaceShip p v xs c
  removeHeart (Jet p v (x:xs) c) = Jet p v xs c
  removeHeart (MotherShip p v (x:xs) c) = MotherShip p v xs c
  isDead (Rock _ _ [] _) = True
  isDead (SpaceShip _ _ [] _) = True
  isDead (Jet _ _ [] _) = True
  isDead (MotherShip _ _ [] _) = True
  isDead (Rock _ _ a _) = False
  isDead (SpaceShip _ _ a _) = False
  isDead (Jet _ _ a _) = False
  isDead (MotherShip _ _ a _) = False
  destroy (Rock p v x c) = Rock p v [] c
  destroy (SpaceShip p v x c) = SpaceShip p v [] c
  destroy (Jet p v x c) = Jet p v [] c
  destroy (MotherShip p v x c) = MotherShip p v [] c


removeEnemies :: [Enemy] -> [Enemy]
removeEnemies [] = []
removeEnemies (x@(Rock p v [] c):xs) = if (c < 50) then (Rock p v [] (c+1)) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(SpaceShip p v [] c):xs) = if c < 50 then (SpaceShip p v [] (c+1)) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(Jet p v [] c):xs)  = if c <50 then (Jet p v [] (c+1)) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(MotherShip p v [] c):xs)  = if c < 50 then (MotherShip p v [] (c+1)) : removeEnemies xs else removeEnemies xs                               
removeEnemies (x : xs) =  x: removeEnemies xs


endGame :: GameState -> GameState
endGame = undefined

pauseGame :: GameState -> GameState
pauseGame = undefined

spawnPowerup :: GameState -> GameState
spawnPowerup = undefined

spawnEnemyOrPowerUp :: Float -> GameState -> GameState

spawnEnemyOrPowerUp i g@(GameState (InfoToShow b p e h) k Running m sg) | (m>30000 && i >45) ||(m > 10000 && i > 50) || i > 55 = GameState (InfoToShow b p (fst (randomEnemy g) : e) h) k Running m (snd (randomEnemy g))
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
randomEnemy g@(GameState (InfoToShow _ (Player(Point(x,y)) _ _) _ _) _ _ sc sg) | sc > 50000 && i> 18= (MotherShip (Point p) v [Heart, Heart, Heart,Heart, Heart] 0, s1)
                                                                                | sc > 30000 && i > 12 = (Jet (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                | sc > 0 && i > 7  = (SpaceShip (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                | otherwise = (Rock (Point p) v [Heart] 0, s1)
                                                                                    where (y1, s) = makeRandomCoordinate sg (-screenh + 10) (screenh - 10)
                                                                                          (i, s1) = makeRandomCoordinate s 0 20
                                                                                          p@(a,b) = (screenw-10,y1)
                                                                                          v = normalize (Vector (x- a , y-b))



randomPowerup :: Powerup
randomPowerup = undefined

shootBulletEs :: GameState -> GameState
shootBulletEs g@(GameState i _ _ _ _) = g{infoToShow = shootBulletE 0 [] i}

--doet nog niets met random
shootBulletE :: Float -> [Enemy] -> InfoToShow -> InfoToShow
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(Rock {}): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(SpaceShip _ _ [] _): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(Jet _ _ [] _): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(MotherShip _ _ [] _): es) bul)  =
      shootBulletE random (e: le) (InfoToShow b (Player (Point (x,y)) v h) es bul)
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(SpaceShip (Point(xt, yt)) _ _ _): es) bul) =
      shootBulletE random (e : le) (InfoToShow b (Player (Point (x,y)) v h) es (EnemyBullet (Point (xt - 20, yt) ) (Vector (-1, 0)) : bul))
shootBulletE random le (InfoToShow b (Player (Point(x,y)) v h) (e@(Jet (Point(xt, yt)) _ _ _): es) bul) =
      shootBulletE random (e : le) (InfoToShow b (Player (Point (x,y)) v h) es (EnemyBullet (Point (xt-30, yt)) (normalize (Vector (x-(xt-30), y-yt))) : bul))
shootBulletE random le (InfoToShow b (Player (Point(x,y)) vt h) (e@(MotherShip (Point(xt, yt)) _ _ _): es) bul) =
      shootBulletE random (e : le) (InfoToShow b (Player (Point (x,y)) vt h) es ([EnemyBullet (Point (xt-30, yt)) (normalize (Vector v)), EnemyBullet (Point (xt-30, yt)) (normalize (Vector v1)),EnemyBullet (Point (xt-30, yt)) (normalize (Vector v2))] ++ bul))
      where v@(xv, yv) = (x-(xt-30), y-yt)
            v1@(xv1, yv1) = ((x+20) -(xt-30), (y+20) - yt)
            v2@(xv2, yv2) = ((x-20) -(xt-30), (y-20) - yt)
shootBulletE random le i@(InfoToShow b (Player (Point(x,y)) v _) [] bul) = i{enemies = le}


--if spaceship hits a wall it will go back in the screen, if rock does this it breaks
hittWall :: Enemy -> Enemy
hittWall (SpaceShip p (Vector(dx, dy)) h c) = SpaceShip p (Vector (dx, -dy)) h c
hittWall rock = destroy rock

{-inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState i e Running sc _)
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
-}
inputKey :: Event -> GameState -> GameState
inputKey e gstate = case state gstate of
            Running -> runInput e gstate
            Paused -> pauseInput e gstate
            GameOver -> gOverInput e gstate
            Dead -> deadInput e gstate

runInput :: Event -> GameState -> GameState
runInput (EventKey (SpecialKey KeyEsc) Down _ _) gstate= gstate{state = Paused}
runInput (EventKey (SpecialKey k) Down _ _) gstate@(GameState i e Running sc _) = gstate {infoToShow = handleInputSpecial k i}
runInput (EventKey (Char c) Down _  _) gstate@(GameState i e s sc _) =            gstate { infoToShow = handleInput c i }
runInput _ gstate@(GameState i e s sc _) = gstate

pauseInput :: Event -> GameState -> GameState
pauseInput (EventKey (SpecialKey KeyEsc) Down _ _) gstate = gstate{state = Running}
pauseInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = pauseMouse (x, y) g
pauseInput _ gstate = gstate

gOverInput :: Event -> GameState -> GameState
gOverInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = gOverMouse (x, y) g
gOverInput _ gstate = gstate

deadInput :: Event -> GameState -> GameState
deadInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = deadMouse (x, y) g
deadInput _ gstate = gstate

pauseMouse :: (Float, Float) -> GameState -> GameState
pauseMouse l@(x, y) g | inBox 0 (screenh * 0.5) l = g{state = Running}
                      | inBox 0 0 l = undefined
                      | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                      | otherwise = g

gOverMouse :: (Float, Float) -> GameState -> GameState
gOverMouse l@(x, y) g | inBox 0 (screenh * 0.5) l = initialState (mkStdGen 60)
                      | inBox 0 0 l = undefined
                      | inBox 0 (-screenh * 0.5) l = undefined
                      | otherwise = g

deadMouse :: (Float, Float) -> GameState -> GameState
deadMouse l@(x, y) g | inBox 0 (screenh * 0.5) l = initialState (mkStdGen 60)
                     | inBox 0 0 l = undefined
                     | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                     | otherwise = g


                   

inBox :: Float -> Float -> (Float, Float) -> Bool
inBox dx dy (x, y) = x > dx - bw && x < bw + dx &&
                     y > dy - bh && y < bh + dy
                    where bw = screenw * 0.25
                          bh = screenh * 0.1
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