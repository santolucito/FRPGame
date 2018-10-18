{-# LANGUAGE PartialTypeSignatures #-}
module GameLogic.Movement where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens
import qualified Data.HashSet as S

import GameLogic.Collisions

import FRP.Yampa (Time,random)

import qualified Settings
import Debug.Trace
  
--TODO make continuos time based motion
useInput :: _ -> GameState
useInput (gameState,input,dt) = case input of
  None -> set (board.player1.gameObj.inMotion) False gameState
  Enter -> set (board.player1.gameObj.inMotion) False gameState
  Space -> set (board.player1.gameObj.inMotion) False gameState
  Pause -> set (board.player1.gameObj.inMotion) False gameState
  mvmtDir -> over (board.player1.gameObj) (moveObj dt mvmtDir gameState) gameState 
  --TODO dir is the catch all
  --everytime i add a new key i need to add it here
  --direciton and general key presses should be seperated

-- TODO doesnt reset the randGen until the move is over
-- so every ai uses the same randGen within each move 
-- this means if the ai ever overlap, they get stuck together
aiMove :: (GameState, Time) -> GameState
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
-- | Check two moves ahead in case we get stuck due to rounding error or something (one pixel is not enuf to block)
-- TODO should explicity check for stuck at every turn, or figure out how to not get stuck in the first place
moveObj :: Double -> Direction -> GameState -> GameObj -> GameObj
moveObj dt d gs o = let
  m = makeMove dt d 
 in
   if wallCollision gs (m o) && wallCollision gs (m (m o)) 
   then o else m o


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

