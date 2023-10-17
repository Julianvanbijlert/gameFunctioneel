module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

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
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
-- main = play window backgroundColor fps initialGame gameAsPicture transformGame (\\_ ->id)

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 130

initialGame :: World
initialGame = undefined

gameAsPicture :: World -> Picture
gameAsPicture = undefined

transformGame :: Graphics.Gloss.Interface.IO.Game.Event -> World -> World
transformGame = undefined

nextFrame :: Float -> World -> World
nextFrame = undefined





