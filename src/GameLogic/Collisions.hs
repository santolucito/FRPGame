module GameLogic.Collisions where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs
import Render.ImageIO

import Utils

import Control.Lens
import Data.Traversable
import Codec.Picture 
import qualified Data.HashSet as S

import qualified GameObjs.Lamp
import qualified GameObjs.Coin
import qualified GameObjs.Ghost

import Data.Maybe (fromMaybe)

import Debug.Trace


findObjCollisions :: GameState -> GameState
findObjCollisions g = let
  (g',objs') = mapAccumL 
                 updateCollide --update the collision of every object
                 (set (interface.active) False g) --and along the way track if any collision is turning on/off interface in gamestate
                 (S.toList $ S.filter _display $ view (board.objs) g)
 in 
  set (board.objs) (S.fromList objs') g'

-- | If any of the pixels of an object (rn just the positon, not all pixels) overlap with 
--   any of the positions of the player, generate a new state and object
--   TODO, why not just return the new gameState?
updateCollide :: GameState -> GameObj -> (GameState,GameObj)
updateCollide g o = 
  if S.empty /= S.intersection (S.fromList $ objCollider g o) (S.fromList $ objCollider g (view (board.player1.gameObj) g))
  then case _name o of --TOOD need multiway if to remove hardcoded strings?
       "ghost" ->  GameObjs.Ghost.ghostCollide o g
       "coin" -> GameObjs.Coin.coinCollide o g 
       "lamp" -> GameObjs.Lamp.lampCollide o g
       _ -> (traceShow  $ "unhandled collision with"++_name o) (g,o)
  --if we have not collided with anything, turn off the interface (probably will need to change this at some point)
  --TODO somehow need a cleaner approach for this
  else (set (interface.active) (False || (_active._interface) g) g,o) 

wallCollision :: GameState -> GameObj -> Bool
wallCollision gs o = let
  walls = pixelData $ getImg gs $ view (board.level) gs
  boardPixels = map (\(x,y) -> pixelAtFromCenter walls x y) (objCollider gs o)
 in 
  any (==blackAPixel) (boardPixels)

pixelAtFromCenter :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
pixelAtFromCenter i x y = let
  h = imageHeight i 
  w = imageWidth i 
  x' = (w `div` 2) + x
  y' = (h `div` 2) + (-y)
 in
  pixelAt i 
    (max 0 $ min (imageWidth i-1) x')
    (max 0 $ min (imageHeight i-1) y')
  --whitePixel


-- | Get the outline of the rectangle of the game obj
--   TODO for pixel level detection - get outline of img of game obj
--   to get outline, maybe require a second img? or can i just preprocess the images with imgmagik with edge detection?
objBoxCollider :: GameState -> GameObj -> [(Int,Int)]
objBoxCollider g obj = let
  (x,y,xsize,ysize) = objectDims g obj
  xsize' = xsize/2
  ysize' = ysize/2
 in
  [(x',y') | x' <- [floor x-ceiling xsize'.. floor x+ceiling xsize'],
             y' <- [floor y-ceiling ysize', floor y+ceiling ysize']] ++
  [(x',y') | x' <- [floor x-ceiling xsize', floor x+ceiling xsize'],
             y' <- [floor y-ceiling ysize'.. floor y+ceiling ysize']]

-- | try to get a pixel level rep of collision, but fall back to outline of box
objCollider :: GameState -> GameObj -> [(Int,Int)] --TODO migrate HashSet type back up this chain of fxns
objCollider g obj =
  fromMaybe 
    --(trace ("Couldn't find collision image for "++(_name obj)) (objBoxCollider g obj))
    (objBoxCollider g obj)
    (getBlackPixelLocs g obj)
