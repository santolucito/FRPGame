
module Render.Render where

import Data.Monoid
import qualified Data.HashSet as S
import Graphics.Gloss

import Types.GameObjs
import Types.Common
import FRP.Yampa
import Render.ImageIO

import Control.Lens (view)

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr renderState

renderState :: GameState -> Picture
renderState s = 
  placeBkgd s <>
  placePlayer s <>
  (mconcat $ placeGameObjs s) <>
  placeText s

--TODO major refactoring needed here
placeGameObjs :: GameState -> [Picture]
placeGameObjs g = let
   os' = view (board.objs) g :: S.HashSet GameObj
   os = S.filter (_display) os'
   (px,py) = (mapTup realToFrac (view (board.player1.gameObj.position) g) :: (Float,Float))
   myPos o = mapTup realToFrac (_position o)
   myScale o = scale (_scaleFactor o) (_scaleFactor o)
   f o = translate ((fst $myPos o)-px) ((snd $myPos o)-py) $ myScale o $ snd $ getImg g o
 in
   reverse $ map f (S.toList os) -- need to reverse to get ghosts on top for some reason

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

placeText :: GameState -> Picture
placeText g = let
   --TODO move to GameLogic
   t = case _status g of 
     GameOver -> "Game Over!" 
     LevelUp -> "Level Up"
     InProgress -> "Score:"++ (show $ (_score._player1._board) g)
 in 
   translate (-300) 260 $ 
     (color (greyN 0.95) $ rectangleSolid 600 80) <>
     (translate (-90) (-20) $ scale (0.5) (0.5) $ text t)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)
