module Settings where

import Debug.Trace

--levelImageSrc = "pacmanMaze2.png" 
levelImageSrc = "stoneash.png" 
imageDir = "pics/"
fps = 60 :: Int
speed = 200 :: Double --TODO this is for every obj, make speed option by object name


--turn debug output on/off
traceMe  :: Show b => b -> b
traceMe x = traceShow x x
