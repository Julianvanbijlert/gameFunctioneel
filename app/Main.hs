module Main where

import Controller
import Model (initialState, screenh, screenw )
import View
import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), Color, playIO )
import Graphics.Gloss
import System.Random


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
main = do
    playIO window -- Or FullScreen
              backgroundColor  -- Background color
              fps              -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
-- main = play window backgroundColor fps initialGame gameAsPicture transformGame (\\_ ->id)

window :: Display
window = InWindow "Shoot m up" (round Model.screenw * 2, round Model.screenh * 2) (10, 10)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 100







