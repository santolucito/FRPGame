{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}

module InterfaceLogic where

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

-- | How does user input through the interface change the game state
--   pull the funciton from the game state
update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> do
    gs <- arr runArrow -< (gameState,input)
    returnA -< gs
  
-- |  pull out the current interfaceUpdate fxn and apply it the gameState and input
runArrow (gameState, input) = let
  f = case (_status gameState) of
        ShowInterface i -> interfaceUpdate i
        _ -> trace "Trying to run interface when game status is not ShowInterface" fst --return the gamestate unchanged
 in
  f (gameState, input)
  
