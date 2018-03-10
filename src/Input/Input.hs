{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Input.Input where

import qualified Types.Common as T

import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Yampa (Event(..), SF, arr, returnA, dHold, accumHoldBy)

import Data.Maybe

import System.IO.Unsafe
import System.Exit
import Debug.Trace

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event [T.InputEvent]) T.GameInput
parseInput = proc keys -> do
  up    <- accumHoldBy (readKey $ G.SpecialKey G.KeyUp) False    -< keys
  down  <- accumHoldBy (readKey $ G.SpecialKey G.KeyDown) False  -< keys
  left  <- accumHoldBy (readKey $ G.SpecialKey G.KeyLeft) False  -< keys
  right <- accumHoldBy (readKey $ G.SpecialKey G.KeyRight) False -< keys
  enter <- accumHoldBy (readKey $ G.SpecialKey G.KeyEnter) False -< keys
  returnA -< calcDir (up,down,left,right,enter)
 where 
  readKey k lastVal events = if 
    | elem (k, G.Up)   $ getK events -> False
    | elem (k, G.Down) $ getK events -> True 
    | otherwise                      -> lastVal
  getK = mapMaybe getK'
  getK' (G.EventKey k d _ _ ) = Just (k,d)
  getK' _ = Nothing

  calcDir (up,down,left,right,enter) = if 
    | enter         ->  T.Enter --TODO this isnt really a direction...
    | up && left    ->  T.UpLeft
    | up && right   ->  T.UpRight
    | down && left  ->  T.DownLeft
    | down && right ->  T.DownRight
    | up            ->  T.Up
    | down          ->  T.Down
    | left          ->  T.Left
    | right         ->  T.Right
    | True          ->  T.None
