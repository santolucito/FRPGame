module Utils where

import Types.GameObjs
import Types.Common
import Render.ImageIO


import Data.Maybe
import Data.List
import qualified Data.Map as M
import Codec.Picture

import Debug.Trace

-- | conveniences
showInterface :: GameState -> Bool
showInterface = _active. _interface

getInterfaceText :: GameState -> String
getInterfaceText = _displayText. _interface

isGameOver :: GameState -> Bool
isGameOver g = _status g == GameOver

leveledUp :: GameState -> Bool
leveledUp g = _status g == LevelUp

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)

-- | Get the location of the black pixels in an image
--   Used for collision detection
getBlackPixelLocs :: GameState -> GameObj -> Maybe [(Int,Int)]
getBlackPixelLocs g o = let
  is = _images g
  iData = M.lookup ("pics/"++(_collisionImg o)) is
  (x,y,xsize,ysize) = objectDims g o
  getLocs i = map (\(x',y') -> (floor x+x'-(floor$ xsize/2),floor y+y'-(floor $ ysize/2))) $ nub $ map (mapTup (\v -> floor $ fromIntegral v*_scaleFactor o)) $ catMaybes $ map (\(x,y) -> if (pixelAt i x y == blackAPixel) then Just (x,y) else Nothing) [(x,y) | x <- [0..imageWidth i-1], y <- [0..imageHeight i-1]]
 in
  case iData of
    Nothing -> Nothing
    Just i -> Just $ getLocs $ fst i

-- | Inspired by superCollider
-- map a value from one linear range to another linear range
{-linlin :: Fractional a => a -> (a,a) -> (a,a) -> a
linlin (oldMin,oldMax) (newMin,newMax) v = let
  scalingFactor = (newMax - newMin) / (oldMax - oldMin)
  offset = newMin - (scalingFactor * oldMin)
 in
  v*scalingFactor + offset
-}

-- | all positions are from center of image 
-- IMPT: this is different than some (but not all) image libraries, which start (0,0) at the top left corner
-- you might need to do some (-xsize/2) kind of operations
-- This should be unnecessary once we are using the image based colliders
objectDims :: GameState -> GameObj -> (Double,Double,Double,Double)
objectDims g o = let
  objImg = fst $ getImg g o
  (x,y) = _position o
  sizeBy f = realToFrac $ _scaleFactor o * (fromIntegral $ f objImg)
 in
  (x,y,sizeBy imageWidth,sizeBy imageHeight)
  

