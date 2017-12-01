{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}

module GameLogic where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs
import Render.ImageIO
import Types.HasImage

import Control.Lens
import Data.Traversable
import Codec.Picture 
import qualified Data.HashSet as S

import FRP.Yampa

import qualified Settings
import Debug.Trace

update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> 
  do
    rec 
      t <- time -< ()
      t' <- iPre 0 -< t
    gs <- arr (uncurry trackTime) -< (t, gameState)
    moved <- arr useInput -< (gs,input,t-t')
    collisons <- arr findObjCollisions -< moved 
    returnA -< collisons
  where
    --TODO make continuos time based motion
    useInput (gameState,input,dt) = case input of
         None -> if view (board.player1.inMotion) gameState
                 then move dt (view (board.player1.dir) gameState) gameState --keep moving the same direction as before
                 else set (board.player1.inMotion) False gameState --dont move in the beginning
         dir -> move dt dir gameState

trackTime :: Time -> GameState -> GameState
trackTime t g = 
  over (board.player1) (updatePlayerGif t) g

-- | Check two moves ahead in case we get stuck due to rounding error or something (one pixel is not enuf to block)
-- TODO should explicity check for stuck at every turn, or figure out how to not get stuck in the first place
move :: Double -> Direction -> GameState -> GameState
move dt d g = let
  m = makeMove dt d
 in
   if wallCollision (m g) && wallCollision (m (m g)) 
   then g else m g

--TODO maybe still relatively expensive in toList everytime, maybe cache or use fancy lenses?
findObjCollisions :: GameState -> GameState
findObjCollisions g = let
  (g',objs') = mapAccumL updateCollide g (filter _display $ S.toList $ view (board.objs) g)
 in 
  set (board.objs) (S.fromList objs') g'

--what happens to an obj when the player collides with it
updateCollide :: GameState -> GameObj -> (GameState,GameObj)
updateCollide g o = 
  if elem ((\(x,y)->(floor x,floor y)) $ _position o) (playerLocs g)
  then (over (board.player1.score) (+1) g,
        set display False o)
  else (g,o)

--what happens to the player when it collides with an obj
--playerCollideUpdate :: ??

didCollide :: GameState -> GameObj -> GameObj -> Bool
didCollide gs g1 g2 = undefined

wallCollision :: GameState -> Bool
wallCollision g = let
  walls = fst $ getImg g $ view (board.levelName) g
  boardPixels = map (\(x,y) -> pixelAtFromCenter walls x y) (playerLocs g)
 in 
  any (==blackAPixel) (boardPixels)

--TODO for pixel level detection
--rather than building rect, get positions of all nonalpha pixels
playerLocs :: GameState -> [(Int,Int)]
playerLocs g = let
  player = view (board.player1.gameObj) g
  (x,y,xsize,ysize) = objectDims g player
  xsize' = xsize/2
  ysize' = ysize/2
 in
  [(x',y') | x' <- [floor x-ceiling xsize'.. floor x+ceiling xsize'],y' <- [floor y-ceiling ysize'.. floor y+ceiling ysize']]

--all positions are from center of image
objectDims :: GameState -> GameObj -> (Double,Double,Double,Double)
objectDims g o = let
  objImg = fst $ getImg g o
  (x,y) = _position o
  sizeBy f = realToFrac $ _scaleFactor o * (fromIntegral $ f objImg)
 in
  (x,y,sizeBy imageWidth,sizeBy imageHeight)
  
pixelAtFromCenter :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
pixelAtFromCenter i x y = let
  h = imageHeight i 
  w = imageWidth i 
  x' = (w `div` 2) + x
  y' = (h `div` 2) + (-y)
 in
  pixelAt i x' y'
  --whitePixel

makeMove :: Double -> Direction -> GameState -> GameState
makeMove dt d g = let 
    s = Settings.speed*dt
    updateF = case d of
     Down  -> (0,-s)
     Up    -> (0,s)
     Left  -> (-s,0)
     Right -> (s,0)
     _     -> (0,0)
    appT (dx,dy) (x,y) = (x+dx,y+dy)
    newPos = over (board.player1.gameObj.position) (appT updateF) g
    newDir = set (board.player1.dir) d newPos
  in
    set (board.player1.inMotion) True newDir

-- | conviences

isGameOver :: GameState -> Bool
isGameOver s = False

traceMe x = traceShow x x
