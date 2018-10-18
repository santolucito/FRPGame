{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}

module InterfaceLogic where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import FRP.Yampa

-- | How does user input through the interface change the game state
--   pull the funciton from the game state
update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> do
    gs <- arr runArrow -< (gameState,input)
    returnA -< gs
  
-- |  pull out the current interfaceUpdate fxn and apply it the gameState and input
runArrow :: (GameState, GameInput) -> GameState
runArrow (gameState, input) = let
  f = (_interfaceUpdate$ _interface gameState) 
 in
  f (gameState, input)
  
