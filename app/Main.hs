module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), Color, playIO )
import Graphics.Gloss
import System.Random

main :: IO ()
main = do
    
    x <- newStdGen
    h <- readScores 
    length h `seq` writeFile "app/Scores.txt" (unlines h)
    
    
    playIO window                   
              backgroundColor       -- Background color
              fps                   -- Frames per second
              (initialState x h)    -- Initial state
              view                  -- View function
              input                 -- Event function
              step                  -- Step function

window :: Display
window = let size = 10 in InWindow "Shoot m up" (round Model.screenw * 2, round Model.screenh * 2) (size, size)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60







