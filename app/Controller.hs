{-# LANGUAGE MultiParamTypeClasses #-}

module Controller where
  -- | This module defines how the state changes
  --   in response to time and user input

import Model
import Data.Char (isDigit)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Color
import Data.Maybe (catMaybes)
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState { rndGen = sg})
    | elapsedTime gstate + secs > numberOfSecsBetweenActions = --nog random toevoegen
      readWriteScores . spawnEnemy (fst j) . shootBulletEs . checkState $ gstate{rndGen = snd j} { elapsedTime = 0}
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
    where p0            = collideFunctionBoard p o -- checks if player is out of the screen
          (p1, e1, sc1) = collideFunctions p0 e sc -- checks if player is hit by a enemie
          (p2, b1, sc2) = collideFunctions p1 b sc1 -- checks if player is hit by a bullet
          (e2, b2, sc3) = collideFunctionEnemy e1 b1 sc2 -- checks if an enemie is hit by a bullet
          e3            = filter (not.(`collides` o)) e2 -- removes enemies if enemie out of screen
          e4            = removeEnemies e3 -- remove all enemies that died
          b3            = removeBullets b2 -- removes all bullets out of screen

-- | checks for every enemy if it collides and updates the enemy hearts and the score
collideFunctionEnemy :: (Collides s a, Remove s, Num score) => [s] -> [a] -> score -> ([s], [a], score)
collideFunctionEnemy e [] sc = (e, [], sc) -- If there are no objects a there are no collisions
collideFunctionEnemy [] b sc = ([], b, sc) -- If there are no objects s there are no collisions
-- With help from recursion there is checked for each enemy whether there is collissions
-- Then the updated list of objects a and score is given along to calculate the rest of the enemies
collideFunctionEnemy (e:es) b sc = let (ys, zs, sc0) = collideFunctionEnemy es b1 s in (e1:ys, zs, sc0) 
                                     where (e1, b1, s) = collideFunctions e b sc -- Calculates the enemy collision, the list of objects a and the score

-- | Checks if the player has collided with the board
  -- This is not in the Collides class since it doesn't need to return a score or has a list of borders as input
collideFunctionBoard :: (Collides s a, Remove s) => s -> a -> s
collideFunctionBoard p b | collides p b = destroy p
                         | otherwise    = p

-- | Moves all enemies and bullets
moveEverything :: GameState -> GameState
moveEverything gstate@(GameState {infoToShow = InfoToShow b p xs bs}) = gstate{infoToShow = InfoToShow b p enem bul}
                                                                       where enem = moveAllEnemies p xs
                                                                             bul  = moveAllBullets bs

-- | Checks if player is dead and changes state
checkDead :: GameState -> GameState
checkDead gstate@(GameState {infoToShow = InfoToShow _ p _ _, state = running})  | isDead p  = gstate{state = Dead}
                                                                                 | otherwise = gstate

moveAllEnemies :: Player -> [Enemy] -> [Enemy]
moveAllEnemies p = map (moveEnemy p)

-- | Moves the enemy according there type
moveEnemy :: Player -> Enemy -> Enemy
-- enemy is death if they have no hearts so the enemy stands still
moveEnemy _ e@(Rock {enemyLives = []})       = e
moveEnemy _ e@(SpaceShip {enemyLives = []})  = e
moveEnemy _ e@(Jet {enemyLives = []})        = e
moveEnemy _ e@(MotherShip {enemyLives = []}) = e
-- enemy moves along its set vector
moveEnemy _ e@(SpaceShip { enemyPos = (Point(x, y)), enemyDir = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
moveEnemy _ e@(Rock { enemyPos = (Point(x, y)), enemyDir      = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
moveEnemy _ e@(Jet { enemyPos = (Point(x, y)), enemyDir       = (Vector(dx, dy))}) =  e{enemyPos = Point (x + dx, y + dy ), enemyDir = Vector (dx, dy)}
-- the mothership moves towards the player, so their vector is calculated based on the player movement 
moveEnemy (Player {pos = (Point(xt,yt))}) e@(MotherShip { enemyPos = (Point(x, y))}) =  e{enemyPos = Point (x + a, y + b ), enemyDir = v}
                                                                                         where v@(Vector (a,b)) =  normalize (Vector (xt - x , yt-y))

moveAllBullets :: [Bullet] -> [Bullet]
moveAllBullets = map moveBullet

-- | Moves the bullet according to their vector
moveBullet :: Bullet -> Bullet
moveBullet b@((EnemyBullet  { bulletPos = (Point(x, y)), bulletDir = (Vector(dx, dy))})) = b{bulletPos = Point (x + 2*dx, y + 2*dy), bulletDir = Vector (dx, dy)}
moveBullet b@((PlayerBullet { bulletPos = (Point(x, y)), bulletDir = (Vector(dx, dy))})) = b{bulletPos = Point (x + 2*dx, y + 2*dy), bulletDir = Vector (dx, dy)}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | type class that contains for s enemy and player and a bullet enemy and border
-- | An interface for checking whether they have collided and 
-- | acting on the checks of collision against a list of type a
class Collides s a where
  collides :: s -> a -> Bool
  collideFunctions :: Num score => s -> [a] -> score -> (s, [a], score)
instance Collides Enemy Bullet where
  collides e (EnemyBullet {}) = False
  -- If theyre dead the bullet goes through them
  collides (Rock {enemyLives = []}) _       = False
  collides (SpaceShip {enemyLives = []}) _  = False
  collides (Jet {enemyLives = []}) _        = False
  collides (MotherShip {enemyLives = []}) _ = False
  -- checks if the bullet hit them according to the position of the bullet the posistion and size of the enemy
  collides (Rock {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))})       = let xSize =17 
                                                                                                 ySize = 20 
                                                                                                  in a>=x-xSize &&a<=x+xSize&& b>=y-ySize &&b<=y+ySize
  collides (SpaceShip {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))})  = let size = 10 
                                                                                                  in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collides (Jet {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))})        = let size = 7 
                                                                                                  in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collides (MotherShip {enemyPos = Point(x, y)}) (PlayerBullet {bulletPos = (Point(a,b))}) = let size = 20 
                                                                                                  in a>=x-size &&a<=x+size&& b>=y-size &&b<=y+size
  collideFunctions e [] sc = (e, [], sc)
  -- the functions goes over the list using recursion checking if they collide,  
  -- if they collide a heart is deducted from the enemie and the bullet will be removed
  -- there will be points added to the score according to how difficult the enemy is
  collideFunctions r@(Rock {}) (x:xs) sc      | collides r x = let points = 100 in  collideFunctions (removeHeart r) xs (sc + points)
                                              | otherwise    = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(SpaceShip {}) (x:xs) sc | collides r x = let points = 200 in  collideFunctions (removeHeart r) xs (sc + points)
                                              | otherwise    = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(MotherShip {}) (x:xs) sc| collides r x = let points = 2000 in  collideFunctions (removeHeart r) xs (sc + points)
                                              | otherwise    = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)
  collideFunctions r@(Jet {}) (x:xs) sc       | collides r x = let points = 500 in  collideFunctions (removeHeart r) xs (sc + points)
                                              | otherwise    = let (q, ys, s) = collideFunctions r xs sc in (q, x:ys, s)

instance Collides Player Bullet where
  collides p (PlayerBullet {}) = False
    -- checks if the bullet hit them according to the position of the bullet the posistion and size of the player
  collides (Player { pos = (Point(x,y))}) (EnemyBullet{bulletPos = (Point(a,b))}) = let xSize = 30
                                                                                        ySize = 10 
                                                                                         in a>=x && a<=x+xSize && b>=y-ySize &&b <=y+ySize
   -- the functions goes over the list using recursion checking if they collide,  
  -- if they collide a heart is deducted from the player and the bullet will be removed
  -- there will be points deducted from the score
  collideFunctions p [] sc = (p, [], sc)
  collideFunctions p (x:xs) sc   | collides p x = let points = 100 in collideFunctions (removeHeart p) xs (sc-points)
                                 | otherwise    = let (q, ys, s) = collideFunctions p xs sc in (q, x:ys, s)

instance Collides Player Enemy where
  -- checks if the player collides with the enemy using the enemy bullet instance
  -- this way you can check for all outerpoints of the player if they collide with the enemy 
  collides (Player { pos = (Point(x,y))}) e = let xSize = 30
                                                  ySize = 10 
                                                   in  collides e (PlayerBullet (Point (x,y-ySize)) (Vector (x,x))) 
                                                    || collides e (PlayerBullet (Point (x,y+ySize)) (Vector (x,x))) 
                                                    || collides e (PlayerBullet (Point (x+xSize,y-ySize)) (Vector (x,x))) 
                                                    || collides e (PlayerBullet (Point (x+xSize,y+ySize)) (Vector (x,x)))
  collideFunctions p [] sc = (p, [], sc)
  -- the functions goes over the list using recursion checking if they collide,  
  -- if they collide a heart is deducted from the player and the enemie will be removed
  -- there will be points deducted from the score
  collideFunctions p (x:xs) sc | collides p x = let points = 200
                                                    (q, ys, s) = collideFunctions (removeHeart p) xs (sc-points) 
                                                     in (q, destroy x:ys, s)
                               | otherwise    = let (q, ys, s) = collideFunctions p xs sc 
                                                     in (q, x:ys, s)
instance Collides Player Border where
  -- checks if the player has collided with the border
  collides (Player { pos = (Point(x,y))}) (Border a b) = let xSize = 30
                                                             ySize = 10 
                                                              in  y+ySize>=a || y-ySize<=b || x<=(-screenw) || x+xSize>=screenw
  collideFunctions p x sc = (p, x, sc)
instance Collides Enemy Border where
  -- checks if the enemy has crossed the border (is on the other side of the border)
  collides (Rock {enemyPos = Point(x, y)}) (Border a b)        = let xSize = 17 
                                                                     ySize = 20 
                                                                      in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh
  collides  (SpaceShip {enemyPos = Point(x, y)}) (Border a b)  = let size = 10 
                                                                     in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collides  (Jet {enemyPos = Point(x, y)}) (Border a b)        = let size = 7 
                                                                     in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collides  (MotherShip {enemyPos = Point(x, y)}) (Border a b) = let size = 20 
                                                                     in x-size>screenw ||x+size<(-screenw)|| y+size<(-screenh) ||y-size>screenh
  collideFunctions p x sc = (p, x, sc)

-- | type class the has Player and Enemie
-- | This is an interface to remove hearts and to check if they are dead
class Remove p where
  removeHeart :: p -> p
  isDead :: p -> Bool
  destroy :: p -> p
instance Remove Player where
  -- removes a heart, if there are no hearts the lives list stays empty
  removeHeart p@(Player {lives = []})     = p
  removeHeart p@(Player {lives = [x]})    = p{lives = []}
  removeHeart p@(Player {lives = (x:xs)}) = p{lives = xs}
  isDead (Player {lives = []}) = True
  isDead p                     = False
  -- takes all hearts away from the player
  destroy p = p{lives = []}
instance Remove Enemy where
  -- removes a heart, if there are no hearts the lives list stays empty
  removeHeart e@(Rock {enemyLives = []})          = e
  removeHeart e@(SpaceShip {enemyLives = []})     = e
  removeHeart e@(Jet {enemyLives = []})           = e
  removeHeart e@(MotherShip {enemyLives = []})    = e
  removeHeart e@(Rock {enemyLives = [x]})         = e {enemyLives = []}
  removeHeart e@(SpaceShip {enemyLives = [x]})    = e {enemyLives = []}
  removeHeart e@(Jet {enemyLives = [x]})          = e {enemyLives = []}
  removeHeart e@(MotherShip {enemyLives = [x]})   = e {enemyLives = []}
  removeHeart e@(Rock {enemyLives =(x:xs)})       = e {enemyLives = xs}
  removeHeart e@(SpaceShip {enemyLives =(x:xs)})  = e {enemyLives = xs}
  removeHeart e@(Jet {enemyLives =(x:xs)})        = e {enemyLives = xs}
  removeHeart e@(MotherShip {enemyLives =(x:xs)}) = e {enemyLives = xs}
  isDead (Rock {enemyLives = []})       = True
  isDead (SpaceShip {enemyLives = []})  = True
  isDead (Jet {enemyLives = []})        = True
  isDead (MotherShip {enemyLives = []}) = True
  isDead e                              = False
  -- takes all hearts away from the enemy
  destroy e = e{enemyLives = []}

-- | when an enemy has died the frame goes up 1 per step 
-- | this frame is being used in showExplosion so the explosion gets bigger
removeEnemies :: [Enemy] -> [Enemy]
removeEnemies [] = []
-- There is recursion used here to avoid having to dubble loop the list with map and filter
-- if the enemy is dead has existed for 50 steps the enemy is removed, otherwice the step gets bigger
removeEnemies (x@(Rock {enemyLives = [], frame = c}):xs)        = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(SpaceShip {enemyLives = [], frame = c}):xs)   = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(Jet {enemyLives = [], frame = c}):xs)         = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x@(MotherShip {enemyLives = [], frame = c}):xs)  = let maxFrame = 50 in if c < maxFrame then x{frame = c+1} : removeEnemies xs else removeEnemies xs
removeEnemies (x : xs)                                          =  x: removeEnemies xs 

-- | checks if the bullets are off screen and removes them if so
removeBullets :: [Bullet]->[Bullet]
removeBullets = filter (not.f)
        where f (PlayerBullet {bulletPos = (Point(x,y))}) = let xSize = 3
                                                                ySize = 2
                                                                 in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh
              f (EnemyBullet  {bulletPos = (Point(x,y))}) = let xSize = 3
                                                                ySize = 2
                                                                 in x-xSize>screenw ||x+xSize<(-screenw)|| y+ySize<(-screenh) ||y-ySize>screenh

-- | Spawns an enemies
spawnEnemy :: Float -> GameState -> GameState
-- spawns an enemy based on if a random float is higher then a certain number
-- and when the score is higher the the number is lower
-- when there are no enemies it always spawns a enemy
spawnEnemy i g@(GameState {infoToShow = InfoToShow b p e h, state= Running, score = m, rndGen = sg}) | let (highestScore, lowerLimit, mediumScore, middleLimit, highestLimit) = (30000, 45, 1000, 50, 55)
                                                                                                                 in (m>highestScore && i >lowerLimit) ||(m > mediumScore && i > middleLimit) || i > highestLimit || null e
                                                                                                                  = g{infoToShow = InfoToShow b p (fst (randomEnemy g) : e) h, rndGen = snd (randomEnemy g)}
                                                                                                     | otherwise  = g                                  
spawnEnemy _ g = g

-- | Gives a random number and the new StdGen back
makeRandomCoordinate :: StdGen -> Float -> Float -> (Float, StdGen)
makeRandomCoordinate g x y = randomR (x,y :: Float) g

-- | Mathimatical way to normalize a vector
normalize :: Model.Vector -> Model.Vector
normalize (Vector (x,y)) = Vector (x / p, y / p)
          where p = sqrt (x*x + y*y)

-- | spawns a random enemy
randomEnemy :: GameState-> (Enemy, StdGen)
-- based on a random float checks which enemy to spawn and on another random float where they spawn
-- jets and motherships spawn later in the game
randomEnemy g@(GameState {infoToShow = InfoToShow (Border m n) (Player(Point(x,y)) _ _) _ _, score = sc, rndGen = sg})
                                                                                | sc > highestScore && i> higherLimit = (MotherShip (Point p) v [Heart, Heart, Heart,Heart, Heart] 0, s1)
                                                                                | sc > mediumScore && i > mediumLimit = (Jet (Point p) v [Heart,Heart, Heart] 0, s1)
                                                                                | i > lowestLimit                     = (SpaceShip (Point p) v [Heart,Heart,Heart] 0, s1)
                                                                                | otherwise                           = (Rock (Point p) (Vector(-1,0)) [Heart] 0, s1)
                                                                                    where (highestScore, higherLimit, mediumScore, mediumLimit, lowestLimit) = (30000, 18, 15000, 12, 7)
                                                                                          (y1, s) = makeRandomCoordinate sg m n
                                                                                          (i, s1) = let (minRange, maxRange) = (0,20) in makeRandomCoordinate s minRange maxRange
                                                                                          p@(a,b) = let border = 10 in (screenw-border,y1)
                                                                                          v       = normalize (Vector (x-a , y-b))



-- | spawns bullets from the enemies
shootBulletEs :: GameState -> GameState
shootBulletEs g@(GameState {infoToShow = InfoToShow b p e bul, state = Running}) = g{infoToShow = InfoToShow b p e (catMaybes (concatMap (shootBulletE p) e) ++ bul)}
shootBulletEs g = g

-- | spawns a bullet from an enemy if they can shoot bullets
shootBulletE :: Player -> Enemy -> [Maybe Bullet]
-- rock and death enemies can not shoot so they return Nothing
shootBulletE _ (Rock {})  =
      [Nothing]
shootBulletE _ (SpaceShip {enemyLives = []})  =
      [Nothing]
shootBulletE _ (Jet {enemyLives = []})  =
      [Nothing]
shootBulletE _ (MotherShip {enemyLives = []})  =
      [Nothing]
-- the spaceship can only shoot straight ahead
shootBulletE _ (SpaceShip {enemyPos = (Point(xt, yt))}) = let spawnDistance = 20 in
      [Just (EnemyBullet (Point (xt - spawnDistance, yt) ) (Vector (-1, 0)))]
-- the jet shoots straight at the current position of the player
shootBulletE (Player {pos = (Point(x,y))}) (Jet {enemyPos = (Point(xt, yt))}) = let spawnDistance = 30 in
      [Just (EnemyBullet (Point (xt-spawnDistance, yt)) (normalize (Vector (x-(xt-spawnDistance), y-yt))))]
-- the mothership shoots three bullets all a few degrees apart towards the player
shootBulletE (Player {pos = (Point(x,y))}) (MotherShip {enemyPos = (Point(xt, yt))}) =
      [Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v)))
            , Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v1)))
            , Just (EnemyBullet (Point (xt-spawnDistanceX, yt)) (normalize (Vector v2)))]
      where spawnDistanceX = 30
            degreesRotation = 20
            -- the three vectors of the bullets one directly towards player and two with degreesRotation next to that one
            v@(xv, yv) = (x-(xt-spawnDistanceX), y-yt)
            v1@(xv1, yv1) = ((x+degreesRotation) -(xt-spawnDistanceX), (y+degreesRotation) - yt)
            v2@(xv2, yv2) = ((x-degreesRotation) -(xt-spawnDistanceX), (y-degreesRotation) - yt)

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
hsgOverMouse l@(Point(x, y)) g@(GameState {hScores = hs, rndGen =  d}) | inBox 0 (-screenh * 0.6) l = (initialState d hs){state = GameOver}
                                                                       | otherwise = g

igOverMouse :: Model.Point -> GameState -> GameState
igOverMouse l@(Point(x, y)) g@(GameState {hScores = hs, rndGen =  d}) | inBox 0 (screenh * 0.5) l = initialState d hs
                       | inBox 0 (-screenh * 0.5) l = g{infoToShow = ShowHighScores}
                       | otherwise = g

-- | if the state is death then this mouse is used
deadMouse :: Model.Point -> GameState -> GameState
deadMouse l@(Point(x, y)) g@(GameState {score = sc, hScores = hs, rndGen =  d}) | inBox 0 (screenh * 0.5) l = initialState d hs
                     | inBox 0 0 l = g{state = GameOver, infoToShow = ShowHighScores, hScores = show sc : hs} --Zorg dat het ook echt saved
                     | inBox 0 (-screenh * 0.5) l = g{state = GameOver}
                     | otherwise = g

-- | checks if the given point is in box 
inBox :: Float -> Float -> Model.Point -> Bool
inBox dx dy (Point(x, y)) = x > dx - bw && x < bw + dx &&
                     y > dy - bh && y < bh + dy
                    where bw = screenw * 0.25
                          bh = screenh * 0.1

-- | Dit is een functie die inputs handled-

handleInput :: Char -> InfoToShow -> InfoToShow
handleInput 'w' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y + dy)}}
handleInput 's' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y - dy)}}
handleInput 'a' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x - dx,y)}}
handleInput 'd' i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x + dx,y)}}
handleInput 'f' i@(InfoToShow {player = p@(Player{pos = (Point (x,y))}), bullets = bul}) = i{ bullets = PlayerBullet (Point (x + distanceFromP, y)) (Vector (5, 0)) : bul }
      where distanceFromP = 40
handleInput _ i = i

handleInputSpecial :: SpecialKey -> InfoToShow -> InfoToShow
handleInputSpecial KeyUp    i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y + dy)}}
handleInputSpecial KeyDown  i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x,y - dy)}}
handleInputSpecial KeyLeft  i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x - dx,y)}}
handleInputSpecial KeyRight i@(InfoToShow {player = p@(Player{pos = (Point (x,y)), dir = (Vector(dx, dy))})}) = i{player = p{pos = Point (x + dx,y)}}
handleInputSpecial KeySpace i@(InfoToShow {player = p@(Player{pos = (Point (x,y))}), bullets = bul}) = i{ bullets = PlayerBullet (Point (x + distanceFromP, y)) (Vector (5, 0)) : bul }
      where distanceFromP = 40
handleInputSpecial _ i = i


--if the highscores in files is the same as current than go on
-- if its not rewrite the highscores in the file
readWriteScores :: GameState -> IO GameState
readWriteScores gstate@(GameState {hScores = hs})= do
  scores <- readScores
  if scores == hs then return gstate
  else length scores `seq` writeScore gstate


--checks the code for words integers and returns a list of those
readScores :: IO [String]
readScores = do
    contents <- readFile "app/Scores.txt"
    let scores = lines contents
    let integerScores = filter (all isDigit) scores
    let sortedScores = orderList integerScores
    return sortedScores

--replaces document with the current score
writeScore :: GameState -> IO GameState
writeScore gstate@(GameState {hScores = hs}) = do
  writeFile "app/Scores.txt" (unlines hs) 
  return gstate

orderList :: [String] -> [String]
orderList = sortBy compareTwo

compareTwo :: String -> String -> Ordering
compareTwo a b = compare (read b :: Int) (read a :: Int)