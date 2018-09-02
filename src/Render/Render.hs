
module Render.Render where

import Data.Monoid
import qualified Data.HashSet as S
import Graphics.Gloss

import Utils
import Types.GameObjs
import Types.Common
import FRP.Yampa
import Render.ImageIO

import Control.Lens (view)

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF (GameState, (Int,Int)) Picture
drawGame = arr renderState

-- | render in 'bottom' to 'top' order
--   (ie placeBkgs is the bottom layer)
-- TODO use getScreenSize and make this IO
renderState :: (GameState, (Int,Int)) -> Picture
renderState (s, displaySize) = pictures [
    placeBkgd s
  , placePlayer s
  , (mconcat $ placeGameObjs s)
  , placeText s displaySize
  , (if showInterface s then placeInterface s displaySize else blank)
  ]

placeGameObjs :: GameState -> [Picture]
placeGameObjs g = let
   objsToDisplay = S.toList $ S.filter (_display) $ view (board.objs) g
   playerPos = view (board.player1.gameObj.position) g
   objScale o = scale (_scaleFactor o) (_scaleFactor o)
   objTrans o = (uncurry translate) $ mapTup realToFrac $ liftTup (-) (_position o) playerPos
   placeObj o = objTrans o $ objScale o $ snd $ getImg g o
 in
   map placeObj objsToDisplay -- need to reverse to get ghosts on top for some reason (edit, seems to be fine without)

--TODO is it really not possible to do this with liftA2 or something?
liftTup f (x,y) (x',y')= (f x x', f y y') 

-- | keep the player centered at all times
--   TODO merge this with placeGameObjs
placePlayer :: GameState -> Picture
placePlayer g = let
   p = snd $ getImg g $ view (board.player1) g
   --(x,y) = mapTup fromIntegral ((view (board.player1.position)) g)
   sf = view (board.player1.gameObj.scaleFactor) g
 in
   translate 0 0 $ scale sf sf p
     
-- | move the background around the player
placeBkgd :: GameState -> Picture
placeBkgd g = let
   bkgd = snd$ getImg g $ view (board.level) g
   (x,y) = mapTup realToFrac$ view (board.player1.gameObj.position) g
 in
   translate (-x) (-y) bkgd

placeText :: GameState -> (Int,Int) -> Picture
placeText g (dsX', dsY') = let
   (dsX,dsY) = (fromIntegral dsX', fromIntegral dsY')
   --TODO move to GameLogic (add text field to GameState?)
   t = case _status g of 
     GameOver -> "Game Over!" 
     LevelUp -> "Level Up"
     InProgress -> "Score:"++ (show $ (_score._player1._board) g)
     ShowInterface _ -> "Paused"
 in 
   translate ((-0.35)*dsX) (0.4*dsY) $ 
     (color (interfaceColor) $ rectangleSolid (0.4*dsX) (0.2*dsY)) <>
     (color black $ rectangleWire (0.4*dsX) (0.2*dsY)) <>
     (translate (-90) (-20) $ scale (0.5) (0.5) $ text t)

placeInterface :: GameState -> (Int,Int) -> Picture
placeInterface g (dsX',dsY') =
   translate 0 ((-0.45)*dsY) $ 
     (color (interfaceColor) $ rectangleSolid (xSize*dsX) (ySize*dsY)) <>
     (color black $ rectangleWire (xSize*dsX) (ySize*dsY)) <>
     (translate (-xSize*0.5*dsX) (ySize*0.5*dsY) $ mconcat $ zipWith drawLine [1..] textLines)
 where
   (dsX,dsY) = (fromIntegral dsX', fromIntegral dsY')
   drawLine ln t = translate 0 ((-ln)*lineHeight) $ scale (0.25) (0.25) t
   lineHeight = 30
   xSize = 0.8
   ySize = 0.2
   textLines = maybe (error "Tried to display Interface in non-interface state") ((map text). lines) (getInterfaceText g)
   

interfaceColor = makeColor 1 1 1 0.8

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)
