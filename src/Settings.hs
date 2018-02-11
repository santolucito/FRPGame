module Settings where

import Debug.Trace

levelImageSrc = "pacmanMaze2.png" 
imageDir = "pics/"
fps = 30 :: Int
speed = 200 :: Double --TODO this is for every obj, make speed option by object name


--turn debug output on/off
traceMe  :: Show b => b -> b
traceMe x = traceShow x x
