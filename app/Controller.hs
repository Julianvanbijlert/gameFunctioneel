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
      InfoToShow(InfoToShow, enemies, ShowHighScores, player, bullets),
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
step secs gstate@(GameState { rndGen = sg})
    | elapsedTime gstate + secs > numberOfSecsBetweenActions = --nog random toevoegen
      readWriteScores . spawnEnemyOrPowerUp (fst j) .  shootBulletEs . checkState $ gstate{rndGen = snd j} { elapsedTime = 0}
    | otherwise = -- Just update the elapsed time
      readWriteScores . checkState $ gstate{ elapsedTime = elapsedTime gstate + secs }
  where
    j = let (minRange, maxRange) = (0,60) in makeRandomCoordinate sg minRange maxRange --makes a random number to decide if a enemy should be spawned

checkState :: GameState -> GameState
checkState gstate@(GameState {state = Running, score = sc}) = collideFunction . moveEverything $ gstate {score = sc + 1 }
checkState gstate = gstate

-- | Checks and handles the collections
collideFunction :: GameState -> GameState
collideFunction gstate@(GameState {infoToShow = InfoToShow o p e b, score = sc}) =
   checkDead gstate{infoToShow = InfoToShow o p2 e4 b3, score = sc3} -- checks if the final state is dead
    where p0 = collideFunctionBoard p o -- checks if player is out of the screen
          (p1, e1, sc1) = collideFunctions p0 e sc -- checks if player is hit by a enemie
          (p2, b1, sc2) = collideFunctions p1 b sc1 -- checks if player is hit by a bullet
          (e2, b2, sc3) = collideFunctionEnemy e1 b1 sc2 -- checks if an enemie is hit by a bullet
          e3 = filter (not.(`collides` o)) e2 -- removes enemies if enemie out of screen
          e4 = removeEnemies e3 -- remove all enemies that died
          b3 = removeBullets b2 -- removes all bullets out of screen

-- | checks for every enemy if it collides and updates the enemy hearts and the score
collideFunctionEnemy :: (Collides s a, Remove s, Num score) => [s] -> [a] -> score -> ([s], [a], score)
collideFunctionEnemy e [] sc = (e, [], sc)
collideFunctionEnemy [] b sc = ([], b, sc)
collideFunctionEnemy (e:es) b sc = let (ys, zs, sc0) = collideFunctionEnemy es b1 s in (e1:ys, zs, sc0)
      where (e1, b1, s) = collideFunctions e b sc -- TODO higher order functie?

collideFunctionBoard :: (Collides s a, Remove s) => s -> a -> s
collideFunctionBoard p b | collides p b = destroy p
                         | otherwise = p

moveEverything :: GameState -> GameState
moveEverything gstate@(GameState {infoToShow = InfoToShow b p xs bs}) = gstate{infoToShow = InfoToShow b p enem bul}
                                                                where enem = moveAllEnemies p xs
                                                                      bul  = moveAllBullets bs
checkDead :: GameState -> GameState
checkDead gstate@(GameState {infoToShow = InfoToShow _ p _ _, state = running})  | isDead p = gstate{state = Dead}
                                                                 | otherwise = gstate


moveAllEnemies :: Player -> [Enemy] -> [Enemy]
moveAllEnemies p = map (moveEnemy p)

moveEnemy :: Player -> Enemy -> Enemy
moveEnemy _ e@(Rock {enemyLives = []}) = e
moveEnemy _ e@(SpaceShip {enemyLives = []}) = e
moveEnemy _ e@(Jet {enemyLives = []}) = e
moveEnemy _ e@(MotherShip {enemyLives = []}) =  e
moveEnemy _ e@(SpaceShip { enemyPos = (Point(x, y)), enemyDir = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
moveEnemy _ e@(Rock { enemyPos = (Point(x, y)), enemyDir = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
moveEnemy _ e@(Jet { enemyPos = (Point(x, y)), enemyDir = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
moveEnemy (Player {pos = (Point(xt,yt))}) e@(MotherShip { enemyPos = (Point(x, y)), enemyDir = (Vector(dx, dy))}) =  e{enemyPos = Point (x + a, y + b ), enemyDir = v}
                where v@(Vector (a,b)) =  normalize (Vector (xt- x , yt-y))

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets = map moveBullet

moveBullet :: Bullet -> Bullet
moveBullet b@((EnemyBullet  { bulletPos = (Point(x, y)), bulletDir = (Vector(dx, dy))})) = b{bulletPos = Point (x + 2*dx, y + 2*dy), bulletDir = Vector (dx, dy)}
moveBullet b@((PlayerBullet  { bulletPos = (Point(x, y)), bulletDir = (Vector(dx, dy))})) = b{bulletPos = Point (x + 2*dx, y + 2*dy), bulletDir = Vector (dx, dy)}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

class Collides s a where
  collides :: s -> a -> Bool
  collideFunctions :: Num score => s -> [a] -> score -> (s, [a], score)
instance Collides Enemy Bullet where
  collides e (EnemyBullet {}) = False
  collides (Rock {enemyLives = []}) _ = False
  collides (SpaceShip {enemyLives = []}) _ = False
  collides (Jet {enemyLives = []}) _ = False
  collides (MotherShip {enemyLives = []}) _ =  False
  collides (Rock {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))}) = let (xSize, ySize) = (17,20) in a>=x-xSize &&a<=x+xSize&& b>=y-ySize &&b<=y+ySize -- | hardcoded en vierkant collision
  collides (SpaceShip {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))}) = let size = 10 in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collides (Jet {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))}) = let size = 7 in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collides (MotherShip {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))}) = let size = 20 in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collideFunctions e [] sc = (e, [], sc)
  collideFunctions r@(Rock {}) (x:xs) sc| collides r x = let points = 100 in  collideFunctions (removeHeart r) xs (sc + points)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(SpaceShip {}) (x:xs) sc| collides r x = let points = 200 in  collideFunctions (removeHeart r) xs (sc + points)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(MotherShip {}) (x:xs) sc| collides r x = let points = 2000 in  collideFunctions (removeHeart r) xs (sc + points)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(Jet {}) (x:xs) sc| collides r x = let points = 500 in  collideFunctions (removeHeart r) xs (sc + points)
                                            | otherwise = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
                                            -- TODO higher order?
instance Collides Player Bullet where
  collides p (PlayerBullet {}) = False
  collides (Player { pos = (Point(x,y))}) (EnemyBullet{bulletPos = (Point(a,b))}) = let (xSize, ySize) = (30,10) in a>=x && a<=x+xSize && b>=y-ySize &&b <=y+ySize
  collideFunctions p [] sc = (p, [], sc)
  collideFunctions p (x:xs) sc| collides p x = let points = 100 in collideFunctions (removeHeart p) xs (sc-points)
                                 | otherwise = let (q, ys, s) = collideFunctions p xs sc in (q, x:ys, s)
                                 -- TODO higher order??
instance Collides Player Enemy where
  collides (Player { pos = (Point(x,y))}) e = let (xSize, ySize) = (30,10) in collides e (PlayerBullet (Point (x,y-ySize)) (Vector (x,x))) || collides e (PlayerBullet (Point (x,y+ySize)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+xSize,y-ySize)) (Vector (x,x))) ||collides e (PlayerBullet (Point (x+xSize,y+ySize)) (Vector (x,x)))
  collideFunctions p [] sc = (p, [], sc)
  collideFunctions p (x:xs) sc | collides p x = let (q, ys, s) = collideFunctions (removeHeart p) xs (sc-200) in (q, destroy x:ys, s)
                               | otherwise = let (q, ys, s) = collideFunctions p xs sc in (q, x:ys, s)
                               -- TODO remove magic number en higher order?
instance Collides Player Border where
  collides (Player { pos = (Point(x,y))}) (Border a b) = let (xSize, ySize) = (30,10) in y+ySize>=a || y-ySize<=b || x<=(-screenw) || x+xSize>=screenw
  collideFunctions p x sc = (p, x, sc)
instance Collides Enemy Border where
  collides (Rock {enemyPos = Point(x, y)}) (Border a b) = let (xSize, ySize) = (17,20) in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh
  collides  (SpaceShip {enemyPos = Point(x, y)}) (Border a b) = let size = 10 in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collides  (Jet {enemyPos = Point(x, y)}) (Border a b) = let size = 7 in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collides  (MotherShip {enemyPos = Point(x, y)}) (Border a b) = let size = 20 in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collideFunctions p x sc = (p, x, sc)

class Remove p where
  removeHeart :: p -> p
  isDead :: p -> Bool
  destroy :: p -> p
instance Remove Player where
  removeHeart p@(Player {lives = []}) = p
  removeHeart p@(Player {lives = [x]}) = p{lives = []}
  removeHeart p@(Player {lives = (x:xs)}) = p{lives = xs}
  isDead (Player {lives = []}) = True
  isDead p = False
  destroy p = p{lives = []}
instance Remove Enemy where
  removeHeart e@(Rock {enemyLives = []}) = e
  removeHeart e@(SpaceShip {enemyLives = []}) = e
  removeHeart e@(Jet {enemyLives = []}) = e
  removeHeart e@(MotherShip {enemyLives = []}) = e
  removeHeart e@(Rock {enemyLives = [x]}) = e {enemyLives = []}
  removeHeart e@(SpaceShip {enemyLives = [x]}) = e {enemyLives = []}
  removeHeart e@(Jet {enemyLives = [x]}) = e {enemyLives = []}
  removeHeart e@(MotherShip {enemyLives = [x]}) = e {enemyLives = []}
  removeHeart e@(Rock {enemyLives =(x:xs)}) = e {enemyLives = xs}
  removeHeart e@(SpaceShip {enemyLives =(x:xs)}) = e {enemyLives = xs}
  removeHeart e@(Jet {enemyLives =(x:xs)}) = e {enemyLives = xs}
  removeHeart e@(MotherShip {enemyLives =(x:xs)}) = e {enemyLives = xs}
  isDead (Rock {enemyLives = []}) = True
  isDead (SpaceShip {enemyLives = []}) = True
  isDead (Jet {enemyLives = []}) = True
  isDead (MotherShip {enemyLives = []}) = True
  isDead e = False
  destroy e = e{enemyLives = []}

removeEnemies :: [Enemy] -> [Enemy]
removeEnemies [] = []
removeEnemies (x@(Rock {enemyLives = [], frame = c}):xs) = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(SpaceShip {enemyLives = [], frame = c}):xs) = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(Jet {enemyLives = [], frame = c}):xs)  = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(MotherShip {enemyLives = [], frame = c}):xs)  = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x : xs) =  x: removeEnemies xs -- TODO higher order functions??


removeBullets :: [Bullet]->[Bullet]
removeBullets = filter (not.f)
        where f (PlayerBullet {bulletPos = (Point(x,y))}) = let (xSize, ySize) = (3,2) in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh
              f (EnemyBullet  {bulletPos = (Point(x,y))}) = let (xSize, ySize) = (3,2) in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh

-- | Spawns only enemies
spawnEnemyOrPowerUp :: Float -> GameState -> GameState
spawnEnemyOrPowerUp i g@(GameState {infoToShow = InfoToShow b p e h, state= Running, score = m, rndGen = sg}) | let (highestScore, lowerLimit, mediumScore, middleLimit, highestLimit) = (30000, 45, 1000, 50, 55)
                                                                                                                 in (m>highestScore && i >lowerLimit) ||(m > mediumScore && i > middleLimit) || i > highestLimit || null e
                                                                                                                  = g{infoToShow = InfoToShow b p (fst (randomEnemy g) : e) h, rndGen = snd (randomEnemy g)}
                                                                                                              | otherwise = g--nog powerup toevoegen                                        
spawnEnemyOrPowerUp _ g = g

makeRandomCoordinate :: StdGen -> Float -> Float -> (Float, StdGen)
makeRandomCoordinate g0 x y = (a, g1)
    where
      (a,g1) =  randomR (x,y :: Float) g0

-- | Mathimatical way to normalize a vector
normalize :: Model.Vector -> Model.Vector
normalize (Vector (x,y)) = Vector (x / p, y / p)
          where p = sqrt (x*x + y*y)

randomEnemy :: GameState-> (Enemy, StdGen)
randomEnemy g@(GameState {infoToShow = InfoToShow (Border m n) (Player(Point(x,y)) _ _) _ _, score = sc, rndGen = sg})
                                                                                | sc > highestScore && i> higherLimit = (MotherShip (Point p) v [Heart, Heart, Heart,Heart, Heart] 0, s1)
                                                                                | sc > mediumScore && i > mediumLimit = (Jet (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                | i > lowestLimit  = (SpaceShip (Point p) v [Heart] 0, s1)
                                                                                | otherwise = (Rock (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                    where (highestScore, higherLimit, mediumScore, mediumLimit, lowestLimit) = (30000, 18, 15000, 12, 7)
                                                                                          (y1, s) = makeRandomCoordinate sg m n
                                                                                          (i, s1) = let (minRange, maxRange) = (0,20) in makeRandomCoordinate s minRange maxRange
                                                                                          p@(a,b) = let border = 10 in (screenw-border,y1)
                                                                                          v = normalize (Vector (x- a , y-b))




shootBulletEs :: GameState -> GameState

shootBulletEs g@(GameState {infoToShow = InfoToShow b p e bul, state = Running}) = g{infoToShow = InfoToShow b p e (catMaybes (concatMap (shootBulletE p) e) ++ bul)}
shootBulletEs g = g

shootBulletE :: Player -> Enemy -> [Maybe Bullet]
shootBulletE _ (Rock {})  =
      [Nothing]
shootBulletE _ (SpaceShip {enemyLives = []})  =
      [Nothing]
shootBulletE _ (Jet {enemyLives = []})  =
      [Nothing]
shootBulletE _ (MotherShip {enemyLives = []})  =
      [Nothing]
shootBulletE _ (SpaceShip {enemyPos = (Point(xt, yt))}) = let spawnDistance = 20 in
      [Just (EnemyBullet (Point (xt - spawnDistance, yt) ) (Vector (-1, 0)))]
shootBulletE (Player {pos = (Point(x,y))}) (Jet {enemyPos = (Point(xt, yt))}) = let spawnDistance = 30 in
      [Just (EnemyBullet (Point (xt-spawnDistance, yt)) (normalize (Vector (x-(xt-spawnDistance), y-yt))))]
shootBulletE (Player {pos = (Point(x,y))}) (MotherShip {enemyPos = (Point(xt, yt))}) =
      [Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v)))
            , Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v1)))
            , Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v2)))]
      where spawnDistanceX = 30
            degreesRotation = 20
            v@(xv, yv) = (x-(xt-spawnDistanceX), y-yt)
            v1@(xv1, yv1) = ((x+degreesRotation) -(xt-spawnDistanceX), (y+degreesRotation) - yt)
            v2@(xv2, yv2) = ((x-degreesRotation) -(xt-spawnDistanceX), (y-degreesRotation) - yt)

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

--first check input on escape to pause the game. This is because specialinputs already get handled 
--and we want to separate this one.             
runInput :: Event -> GameState -> GameState
runInput (EventKey (SpecialKey KeyEsc) Down _ _) gstate= gstate{state = Paused}
runInput (EventKey (SpecialKey k) Down _ _) gstate@(GameState {infoToShow = i}) = gstate {infoToShow = handleInputSpecial k i}
runInput (EventKey (Char c) Down _  _) gstate@(GameState {infoToShow = i}) = gstate { infoToShow = handleInput c i }
runInput (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate@(GameState {infoToShow = i}) = gstate{infoToShow = runMouse (Point(x, y)) i}
runInput _ gstate = gstate

--if esc, change state to running again. if mouse input do the mouseinput function
pauseInput :: Event -> GameState -> GameState
pauseInput (EventKey (SpecialKey KeyEsc) Down _ _) gstate = gstate{state = Running}
pauseInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = pauseMouse (Point(x, y)) g
pauseInput _ gstate = gstate

gOverInput :: Event -> GameState -> GameState
gOverInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = gOverMouse (Point(x, y)) g
gOverInput _ gstate = gstate

deadInput :: Event -> GameState -> GameState
deadInput (EventKey (MouseButton LeftButton) Down _ (x, y)) g = deadMouse (Point(x, y)) g
deadInput _ gstate = gstate

runMouse :: Model.Point -> InfoToShow -> InfoToShow
runMouse l@(Point(x1, y1)) i@(InfoToShow {player = (Player {pos = (Point(x, y)), dir = (Vector(dx, dy)), lives = h}) }) = i{player = Player (getLoc (x,y) (x1, y1) (dx,dy)) (Vector (dx, dy)) h}

getLoc ::(Float, Float) -> (Float, Float)  -> (Float, Float)  -> Model.Point
getLoc (px, py) (mx, my) (dx, dy)  = Model.Point (px + nx * dx, py + ny * dy)
                              where n@(Model.Vector (nx, ny)) = normalize (Vector (mx - px, my - py))
--checks if pressed in two boxes, one for continue and one for exit
pauseMouse :: Model.Point -> GameState -> GameState
pauseMouse l@(Point(x, y)) g | inBox 0 (screenh * 0.5) l = g{state = Running}
                      | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                      | otherwise = g

gOverMouse :: Model.Point -> GameState -> GameState
gOverMouse l@(Point(x, y)) g = case infoToShow g of
  InfoToShow b p xs bs -> igOverMouse l g
  ShowHighScores -> hsgOverMouse l g


hsgOverMouse ::Model.Point -> GameState -> GameState
hsgOverMouse l@(Point(x, y)) g@(GameState _ _ _ _ hs d) | inBox 0 (-screenh * 0.6) l = (initialState d hs){state = GameOver}
                                                    | otherwise = g

igOverMouse :: Model.Point -> GameState -> GameState
igOverMouse l@(Point(x, y)) g@(GameState _ _ _ _ hs d) | inBox 0 (screenh * 0.5) l = initialState d hs
                       | inBox 0 (-screenh * 0.5) l = g{infoToShow = ShowHighScores}
                       | otherwise = g



deadMouse :: Model.Point -> GameState -> GameState
deadMouse l@(Point(x, y)) g@(GameState _ _ _ sc hs d) | inBox 0 (screenh * 0.5) l = initialState d hs
                     | inBox 0 0 l = g{state = GameOver, infoToShow = ShowHighScores, hScores = show sc : hs} --Zorg dat het ook echt saved LOLL
                     | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                     | otherwise = g

--checks if the given point is in box 
inBox :: Float -> Float -> Model.Point -> Bool
inBox dx dy (Point(x, y)) = x > dx - bw && x < bw + dx &&
                     y > dy - bh && y < bh + dy
                    where bw = screenw * 0.25
                          bh = screenh * 0.1

{-Dit is een functie die inputs handled-}

handleInput :: Char -> InfoToShow -> InfoToShow
handleInput 'w' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y + dy)}}
handleInput 's' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y - dy)}}
handleInput 'a' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x + dx,y)}}
handleInput 'd' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x - dx,y)}}
handleInput 'f' i@(InfoToShow {player = p@(Player{pos = (Point (x,y))}), bullets = bul}) = i{ bullets = PlayerBullet (Point (x + distanceFromP, y)) (Vector (5, 0)) : bul }
      where distanceFromP = 40
handleInput _ i = i

handleInputSpecial :: SpecialKey -> InfoToShow -> InfoToShow
handleInputSpecial KeyUp    i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y + dy)}}
handleInputSpecial KeyDown  i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y - dy)}}
handleInputSpecial KeyLeft  i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x + dx,y)}}
handleInputSpecial KeyRight i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x - dx,y)}}
handleInputSpecial KeySpace i@(InfoToShow {player = p@(Player{pos = (Point (x,y))}), bullets = bul}) = i{ bullets = PlayerBullet (Point (x + distanceFromP, y)) (Vector (5, 0)) : bul }
      where distanceFromP = 40
handleInputSpecial _ i = i


--if the highscores in files is the same as current than go on
-- if its not rewrite the highscores in the file
readWriteScores :: GameState -> IO GameState
readWriteScores gstate@(GameState {hScores = hs})= do
  scores <- readScores
  if scores == hs then return gstate
  else length scores `seq`writeScore gstate



readScores :: IO [String]
readScores = do
    contents <- readFile "app/Scores.txt"
    let scores = lines contents
    let scores2 = filter (all isDigit) scores
    return scores2

writeScore :: GameState -> IO GameState
writeScore gstate@(GameState {hScores = hs}) = do
  writeFile "app/Scores.txt" (unlines hs) 
  return gstate

