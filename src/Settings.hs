module Settings where

import Debug.Trace

--levelImageSrc = "pacmanMaze2.png" 
--levelCollisionImageSrc = "pacmanMaze2.png" 
levelImageSrc = "houseMap.png" 
levelCollisionImageSrc = "houseMapLines.png" 
imageDir = "pics/"
fps = 60 :: Int
speed = 200 :: Double --TODO this is for every obj, make speed option by object name

windowSize = (800,600) :: (Int,Int) --TODO I really want this dynamic, but there is no wasy way to just asl GLUT what the current window size is...
fullscreen = False

--turn debug output on/off
traceMe  :: Show b => b -> b
traceMe x = traceShow x x
