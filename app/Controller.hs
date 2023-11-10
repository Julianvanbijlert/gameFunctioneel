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
      State(Paused, GameOver, Running, Dead),
      Bullet(..),
      Enemy(..),
      Player(..),
      InfoToShow(InfoToShow, enemies, ShowHighScores, player),
      GameState(GameState, elapsedTime, score, state, infoToShow, hScores, rndGen), Heart (Heart),Border (Border), screenw, screenh,
      initialState
    )


import Data.Char (isDigit)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Color
import Data.Maybe (catMaybes)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState {rndGen = sg})
    | elapsedTime gstate + secs > numberOfSecsBetweenActions = --nog random toevoegen
      readWriteScores . spawnEnemyOrPowerUp (fst j) .  shootBulletEs . checkState $ gstate{rndGen = snd j}{ elapsedTime = 0}
    | otherwise = -- Just update the elapsed time
      readWriteScores . checkState $ gstate{ elapsedTime = elapsedTime gstate + secs }
  where
    j = makeRandomCoordinate sg 0 60

checkState :: GameState -> GameState
checkState gstate@(GameState {state = Running, score = sc}) = collideFunction . moveEverything $ gstate {score = sc + 1 }
checkState gstate = gstate

-- | Checks and handles the collections
collideFunction :: GameState -> GameState
collideFunction gstate@(GameState {infoToShow = InfoToShow o p e b, score = sc}) =
   checkDead gstate{infoToShow = InfoToShow o p2 e4 b3, score = sc3}
    where p0 = collideFunctionBoard p o -- checks if player is out of the screen
          (p1, e1, sc1) = collideFunctions p0 e sc -- checks if player is hit by a enemie
          (p2, b1, sc2) = collideFunctions p1 b sc1 -- checks if player is hit by a bullet
          (e2, b2, sc3) = collideFunctionEnemy e1 b1 sc2 -- checks if an enemie is hit by a bullet
          e3 = filter (not.(`collides` o)) e2 -- removes enemies if enemie out of screen
          e4 = removeEnemies e3 -- remove all enemies that died
          b3 = removeBullets b2 -- removes all bullets out of screen

-- | checks for every enemie if it collides and updates the enemy hearts and the score
collideFunctionEnemy :: (Collides s a, Remove s, Num score) => [s] -> [a] -> score -> ([s], [a], score)
collideFunctionEnemy e [] sc = (e, [], sc)
collideFunctionEnemy [] b sc = ([], b, sc)
collideFunctionEnemy (e:es) b sc = let (ys, zs, sc0) = collideFunctionEnemy es b1 s in (e1:ys, zs, sc0)
      where (e1, b1, s) = collideFunctions e b sc

collideFunctionBoard :: (Collides s a, Remove s) => s -> a -> s
collideFunctionBoard p b | collides p b = destroy p
                         | otherwise = p

moveEverything :: GameState -> GameState
moveEverything gstate@(GameState {infoToShow = InfoToShow b p xs bs}) = gstate{infoToShow = InfoToShow b p enem bul}
                                                                where enem = moveAllEnemies xs
                                                                      bul  = moveAllBullets bs
checkDead :: GameState -> GameState
checkDead gstate@(GameState {infoToShow = InfoToShow _ p _ _, state = running})  | isDead p = gstate{state = Dead}
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
  collides (Rock _ _ [] _) p = False
  collides (SpaceShip _ _ [] _) p = False
  collides (Jet _ _ [] _) p = False
  collides (MotherShip _ _ [] _) p =  False
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
removeEnemies (x@(Rock p v [] c):xs) = if c < 50 then Rock p v [] (c+1) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(SpaceShip p v [] c):xs) = if c < 50 then SpaceShip p v [] (c+1) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(Jet p v [] c):xs)  = if c <50 then Jet p v [] (c+1) : removeEnemies xs else removeEnemies xs
removeEnemies (x@(MotherShip p v [] c):xs)  = if c < 50 then MotherShip p v [] (c+1) : removeEnemies xs else removeEnemies xs
removeEnemies (x : xs) =  x: removeEnemies xs

removeBullets :: [Bullet]->[Bullet]
removeBullets = filter (not.f)
        where f (PlayerBullet (Point(x,y))_) = x-3>screenw ||x+3<(-screenw)|| y+2<(-screenh) ||y-2>screenh
              f (EnemyBullet (Point(x,y))_) = x-3>screenw ||x+3<(-screenw)|| y+2<(-screenh) ||y-2>screenh

spawnEnemyOrPowerUp :: Float -> GameState -> GameState
spawnEnemyOrPowerUp i g@(GameState {infoToShow = InfoToShow b p e h, state= Running, score = m, rndGen = sg}) | (m>30000 && i >45) ||(m > 10000 && i > 50) || i > 55 = g{infoToShow = InfoToShow b p (fst (randomEnemy g) : e) h, rndGen = snd (randomEnemy g)}
                                                                | otherwise = g--nog powerup toevoegen                                        
spawnEnemyOrPowerUp i g = g

makeRandomCoordinate :: StdGen -> Float -> Float -> (Float, StdGen)
makeRandomCoordinate g0 x y = (a, g1)
    where
      (a,g1) =  randomR (x,y :: Float) g0

normalize :: Model.Vector -> Model.Vector
normalize (Vector (x,y))= Vector (x / p, y / p)
          where p = sqrt (x*x + y*y)

randomEnemy :: GameState-> (Enemy, StdGen)
randomEnemy g@(GameState {infoToShow = InfoToShow _ (Player(Point(x,y)) _ _) _ _, score = sc, rndGen = sg}) | sc > 30000 && i> 18= (MotherShip (Point p) v [Heart, Heart, Heart,Heart, Heart] 0, s1)
                                                                                | sc > 15000 && i > 12 = (Jet (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                | sc > 0 && i > 7  = (SpaceShip (Point p) v [Heart] 0, s1)
                                                                                | otherwise = (Rock (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                    where (y1, s) = makeRandomCoordinate sg (-screenh + 10) (screenh - 10)
                                                                                          (i, s1) = makeRandomCoordinate s 0 20
                                                                                          p@(a,b) = (screenw-10,y1)
                                                                                          v = normalize (Vector (x- a , y-b))



randomPowerup :: Powerup
randomPowerup = undefined

shootBulletEs :: GameState -> GameState
shootBulletEs g@(GameState {infoToShow = InfoToShow b p e bul}) = g{infoToShow = InfoToShow b p e (catMaybes (concatMap (shootBulletE p) e)++bul)}

shootBulletE :: Player -> Enemy -> [Maybe Bullet]
shootBulletE (Player (Point(x,y)) v h) e@(Rock {})  =
      [Nothing]
shootBulletE (Player (Point(x,y)) v h) e@(SpaceShip _ _ [] _)  =
      [Nothing]
shootBulletE (Player (Point(x,y)) v h) e@(Jet _ _ [] _)  =
      [Nothing]
shootBulletE (Player (Point(x,y)) v h) e@(MotherShip _ _ [] _)  =
      [Nothing]
shootBulletE (Player (Point(x,y)) v h) e@(SpaceShip (Point(xt, yt)) _ _ _) =
      [Just (EnemyBullet (Point (xt - 20, yt) ) (Vector (-1, 0)))]
shootBulletE (Player (Point(x,y)) v h) e@(Jet (Point(xt, yt)) _ _ _) =
      [Just (EnemyBullet (Point (xt-30, yt)) (normalize (Vector (x-(xt-30), y-yt))))]
shootBulletE (Player (Point(x,y)) v h) e@(MotherShip (Point(xt, yt)) _ _ _) =
      [Just (EnemyBullet (Point (xt-30, yt)) (normalize (Vector v)))
            , Just (EnemyBullet (Point (xt-30, yt)) (normalize (Vector v1)))
            , Just (EnemyBullet (Point (xt-30, yt)) (normalize (Vector v2)))]
      where v@(xv, yv) = (x-(xt-30), y-yt)
            v1@(xv1, yv1) = ((x+20) -(xt-30), (y+20) - yt)
            v2@(xv2, yv2) = ((x-20) -(xt-30), (y-20) - yt)

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
runInput (EventKey (SpecialKey k) Down _ _) gstate@(GameState i _ _ _ _ _) = gstate {infoToShow = handleInputSpecial k i}
runInput (EventKey (Char c) Down _  _) gstate@(GameState i _ _ _ _ _) =            gstate { infoToShow = handleInput c i }
runInput (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate@(GameState i _ _ _ _ _) = gstate{infoToShow = runMouse (x, y) i}
runInput _ gstate@(GameState i e s sc _ _) = gstate

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

runMouse :: (Float, Float) -> InfoToShow -> InfoToShow
runMouse l@(x1, y1) i@(InfoToShow b (Player (Point(x, y)) (Vector(dx, dy)) h) e bul) = i{player = Player (getLoc (x,y) (x1, y1) (dx,dy)) (Vector(dx, dy)) h}

getLoc ::(Float, Float) -> (Float, Float)  -> (Float, Float)  -> Model.Point
getLoc (px, py) (mx, my) (dx, dy)  = Model.Point (px + nx * dx, py + ny * dy)
                              where n@(Model.Vector (nx, ny)) = normalize (Vector(mx - px, my - py))

pauseMouse :: (Float, Float) -> GameState -> GameState
pauseMouse l@(x, y) g | inBox 0 (screenh * 0.5) l = g{state = Running}
                      | inBox 0 0 l = undefined
                      | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                      | otherwise = g

gOverMouse :: (Float, Float) -> GameState -> GameState
gOverMouse l@(x,y) g = case infoToShow g of
  InfoToShow b p xs bs -> igOverMouse l g
  ShowHighScores -> hsgOverMouse l g


hsgOverMouse ::(Float, Float) -> GameState -> GameState
hsgOverMouse l@(x, y) g@(GameState _ _ _ _ hs d) | inBox 0 (-screenh * 0.6) l = (initialState d hs){state = GameOver}
                                                    | otherwise = g

igOverMouse :: (Float, Float) -> GameState -> GameState
igOverMouse l@(x, y) g@(GameState _ _ _ _ hs d) | inBox 0 (screenh * 0.5) l = initialState d hs
                       | inBox 0 0 l = g{infoToShow = ShowHighScores}
                       | inBox 0 (-screenh * 0.5) l = undefined
                       | otherwise = g



deadMouse :: (Float, Float) -> GameState -> GameState
deadMouse l@(x, y) g@(GameState _ _ _ sc hs d) | inBox 0 (screenh * 0.5) l = initialState d hs
                     | inBox 0 0 l = g{state = GameOver, infoToShow = ShowHighScores, hScores = show sc : hs} --Zorg dat het ook echt saved LOLL
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



readWriteScores :: GameState -> IO GameState
readWriteScores gstate@(GameState _ _ _ _ hs _)= do
  scores <- readScores
  let newHighScores = hs
  if scores == newHighScores then return gstate
  else length scores `seq`writeScore gstate


--readWriteScores :: GameState -> IO GameState
--readWriteScores = readScores

readScores :: IO [String]
readScores = do
    contents <- readFile "app/Scores.txt"
    let scores = lines contents
    let scores2 = filter (all isDigit) scores
    return scores2

writeScore :: GameState -> IO GameState
writeScore gstate@(GameState i t s sc hs sg) = do
  writeFile "app/Scores.txt" (unlines hs)  --( show sc ++ "\n")
  return gstate

