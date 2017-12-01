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

import Debug.Trace

update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> 
  do
    t <- time -< ()
    gs <- arr (uncurry trackTime) -< (t, gameState)
    moved <- arr useInput -< (gs,input)
    collisons <- arr findObjCollisions -< moved 
    returnA -< collisons
  where
    --TODO make continuos time based motion
    useInput (gameState,input) = case input of
         None -> if view (board.player1.inMotion) gameState
                 then move (view (board.player1.dir) gameState) gameState --keep moving the same direction as before
                 else set (board.player1.inMotion) False gameState --dont move in the beginning
         dir -> move dir gameState

trackTime :: Time -> GameState -> GameState
trackTime t g = 
  over (board.player1) (updatePlayerGif t) g

-- | Check two moves ahead in case we get stuck due to rounding error or something (one pixel is not enuf to block)
move :: Direction -> GameState -> GameState
move d g = if wallCollision (makeMove d g) && wallCollision (makeMove d (makeMove d g)) 
           then g else makeMove d g

--TODO maybe still relatively expensive in toList everytime, maybe cache or use fancy lenses?
findObjCollisions :: GameState -> GameState
findObjCollisions g = let
  (g',objs') = mapAccumL updateCollide g (filter _display $ S.toList $ view (board.objs) g)
 in 
  set (board.objs) (S.fromList objs') g'

--what happens to an obj when the player collides with it
updateCollide :: GameState -> GameObj -> (GameState,GameObj)
updateCollide g o = 
  if elem (_position o) (playerLocs g)
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
  xsize' = div xsize 2
  ysize' = div ysize 2
 in
  [(x',y') | x' <- [x-xsize'.. x+xsize'],y' <- [y-ysize'.. y+ysize']]

--all positions are from center of image
objectDims :: GameState -> GameObj -> (Int,Int,Int,Int)
objectDims g o = let
  objImg = fst $ getImg g o
  (x,y) = _position o
  sizeBy f = ceiling $ _scaleFactor o * (fromIntegral $ f objImg)
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

makeMove :: Direction -> GameState -> GameState
makeMove d g = let 
    updateF = case d of
     Down  -> (0,-1)
     Up    -> (0,1)
     Left  -> (-1,0)
     Right -> (1,0)
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
