module Main where

import Graphics.Gloss

data Event = Char 
data World = Undefined 

{- 
Play :: Display	
Display mode.

-> Color	
Background color.

-> Int	
Number of simulation steps to take for each second of real time.

-> world	
The initial world.

-> (world -> Picture)	
A function to convert the world a picture.

-> (Event -> world -> world)	
A function to handle input events.

-> (Float -> world -> world)	
A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

-> IO ()
-}
main :: IO ()
main = display window white (Circle 80)
-- main = play window backgroundColor fps initialGame gameAsPicture transformGame (\\_ ->id)

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

backgroundColor :: Color
backgroundColor = makeColor 255 255 255 255

fps :: Int
fps = 30

initialGame :: World
initialGame = undefined

gameAsPicture :: World -> Picture
gameAsPicture = undefined

transformGame :: Event -> world -> world
transformGame = undefined

nextFrame :: Float -> world -> world
nextFrame = undefined





