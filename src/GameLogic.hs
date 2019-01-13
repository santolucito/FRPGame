{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}

module GameLogic where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs
import Types.HasImage

import Control.Lens

import GameLogic.Collisions
import GameLogic.Movement

import FRP.Yampa

import qualified Data.HashSet as H

update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> 
  do
    rec 
      t <- time -< () --TODO isnt there a way to directly get the delta times out of an arrow?
      t' <- iPre 0 -< t
    gs <- arr (uncurry trackTime) -< (t, gameState)
    movedPlayer <- arr useInput -< (gs,input,t-t')
    movedGhosts <- arr aiMove -< (movedPlayer,t-t')
    --TODO dont need movedPlayer to move ghosts
    --     would be nice to parallelize these arrows
    --     this would require a way to merge two game states (movedPlayer+moveGhosts) 
    --     to pass to the collions calculation
    collisons <- arr findObjCollisions -< movedGhosts
    newgs <- arr (\g-> if view (board.player1.score) g == ((view (board.level.num) g) *10) then g {_status = LevelUp} else g) -< collisons
    returnA -< newgs

trackTime :: Time -> GameState -> GameState
trackTime t g = 
  over (board.objs) (H.map (updateGif t)) $
  over (board.player1.gameObj) (updateGif t) g
