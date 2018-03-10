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

update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> do
    gs <- arr useInput -< (gameState,input)
    returnA -< gs
  
useInput :: _ -> GameState
useInput (gameState,input) = case input of
  Enter -> set status GameOver gameState
  _ -> gameState

