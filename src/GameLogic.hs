{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import Utils
import qualified Settings
import Debug.Trace

update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> 
  do
    rec 
      t <- time -< () --TODO isnt there a way to directly get the delta times out of an arrow?
      t' <- iPre 0 -< t
    gs <- arr (uncurry trackTime) -< (t, gameState)
    movedPlayer <- arr useInput -< (gs,input,t-t')
    movedGhosts <- arr aiMove -< (movedPlayer,t-t')
    collisons <- arr findObjCollisions -< movedGhosts
    newgs <- arr (\g-> if view (board.player1.score) g == ((view (board.level.num) g) *10) then g {_status = LevelUp} else g) -< collisons
    returnA -< newgs
  where
  
--TODO make continuos time based motion
useInput :: _ -> GameState
useInput (gameState,input,dt) = case input of
  None -> set (board.player1.gameObj.inMotion) False gameState
  Enter -> set (board.player1.gameObj.inMotion) False gameState
  dir -> over (board.player1.gameObj) (moveObj dt dir gameState) gameState

-- TODO doesnt reset the randGen until the move is over
-- so every ai uses the same randGen within each move 
aiMove :: _ -> GameState
aiMove (gameState,dt) = let
  isGhost o = (_name o == "ghost")
  moveGhost = S.map (\o-> if isGhost o then moveMe (fromEnum (maxBound::Direction)) (_dir o) o else o)
  (r,g) = random $ _gen gameState :: (Double,_)
  newDir = toEnum $ floor (r*(fromIntegral $ fromEnum (maxBound::Direction)))
  moveMe n d o = 
    if moveObj dt d gameState o == o && n>0 
    then moveMe (n-1) (newDir) o 
    else moveObj dt d gameState o
         --if n /= 0
         --then moveObj dt d gameState o
         --else makeMove dt d o
 in
  set gen g $ over (board.objs) moveGhost gameState

trackTime :: Time -> GameState -> GameState
trackTime t g = 
  over (board.player1.gameObj) (updateGif t) g

-- | Check two moves ahead in case we get stuck due to rounding error or something (one pixel is not enuf to block)
-- TODO should explicity check for stuck at every turn, or figure out how to not get stuck in the first place
moveObj :: Double -> Direction -> GameState -> GameObj -> GameObj
moveObj dt d gs o = let
  m = makeMove dt d 
 in
   if wallCollision gs (m o) && wallCollision gs (m (m o)) 
   then o else m o

--TODO maybe still relatively expensive in toList everytime, maybe cache or use fancy lenses?
findObjCollisions :: GameState -> GameState
findObjCollisions g = let
  (g',objs') = mapAccumL updateCollide g (filter _display $ S.toList $ view (board.objs) g)
 in 
  set (board.objs) (S.fromList objs') g'

-- | If any of the pixels of an object overlap with 
--   any of the positions of the player, generate a new state and object
--   TODO, why not just return the new gameState?
updateCollide :: GameState -> GameObj -> (GameState,GameObj)
updateCollide g o = 
  if elem ((\(x,y)->(floor x,floor y)) $ _position o) (objLocs g (view (board.player1.gameObj) g))
  then case _name o of 
       "ghost" ->  ghostCollide
       "coin" -> coinCollide
       otherwise -> (traceShow  $ "unhandled collision with"++_name o) (g,o)
  else (g,o)
 where
  coinCollide = 
        (over (board.player1.score) (+1) g,
        set display False o)
  ghostCollide =
        (set (status) (ShowInterface "Dont run into the monsters! \nPress Enter to restart") g,
         o)

wallCollision :: GameState -> GameObj -> Bool
wallCollision gs o = let
  walls = fst $ getImg gs $ view (board.level) gs
  boardPixels = map (\(x,y) -> pixelAtFromCenter walls x y) (objLocs gs o)
 in 
  any (==blackAPixel) (boardPixels)

makeMove :: Double -> Direction -> GameObj -> GameObj
makeMove dt d o = let 
    s = Settings.speed*dt
    updateF = case d of
     UpLeft     -> diag (-s,s)
     UpRight    -> diag (s,s)
     DownLeft   -> diag (-s,-s)
     DownRight  -> diag (s,-s)
     Up         -> (0,s)
     Down       -> (0,-s)
     Left       -> (-s,0)
     Right      -> (s,0)
     _          -> (0,0)
    diag (x,y) = (x/1.4,y/1.4)
    appT (dx,dy) (x,y) = (x+dx,y+dy)
    motion = d/=None
    objWithNewPos = over (position) (appT updateF) o
    objWithNewDir = set (dir) d objWithNewPos
  in
    set (inMotion) motion objWithNewDir


--TODO for pixel level detection
--rather than building rect, get positions of all nonalpha pixels (might be a problem for gifs tho)
objLocs ::GameState -> GameObj -> [(Int,Int)]
objLocs g obj = let
  (x,y,xsize,ysize) = objectDims g obj
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


