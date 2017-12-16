{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Input.Input where

import qualified Types.Common as T

import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Yampa (Event(..), SF, arr, returnA, dHold, accumHoldBy)

import System.IO.Unsafe
import System.Exit
import Debug.Trace

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event T.InputEvent) T.GameInput
parseInput = proc keys -> do 
  up    <- accumHoldBy (readKey $ G.SpecialKey G.KeyUp) False    -< keys
  down  <- accumHoldBy (readKey $ G.SpecialKey G.KeyDown) False  -< keys
  left  <- accumHoldBy (readKey $ G.SpecialKey G.KeyLeft) False  -< keys
  right <- accumHoldBy (readKey $ G.SpecialKey G.KeyRight) False -< keys
  returnA -< calcDir (up,down,left,right)
 where
  readKey k lastVal event = case event of  --k can t be passed as a paramneter here for some reason
    (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> traceShow event True
    (G.EventKey k G.Up _ _)   -> False
    _                         -> False
  calcDir (up,down,left,right) = if 
    | up    ->  T.Up
    | down  ->  T.Down
    | left  ->  T.Left
    | right ->  T.Right
    | True  ->  T.None
