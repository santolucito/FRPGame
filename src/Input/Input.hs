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
  enter <- accumHoldBy (readKey $ G.SpecialKey G.KeyEnter) False -< keys
  returnA -< calcDir (up,down,left,right,enter)
 where
  -- events oocasionally come out of order (up/down rather than down/up) causing key to 'remain down'
  -- TODO add timestamps to avoid
  readKey k lastVal event = if 
    | getK event == Just (k, G.Up) -> False
    | getK event == Just (k, G.Down) -> True 
    | otherwise                      -> lastVal
  getK (G.EventKey k d _ _ ) = Just (k,d)
  getK _ = Nothing

  calcDir (up,down,left,right,enter) = if 
    | up && left    ->  T.UpLeft
    | up && right   ->  T.UpRight
    | down && left  ->  T.DownLeft
    | down && right ->  T.DownRight
    | up            ->  T.Up
    | down          ->  T.Down
    | left          ->  T.Left
    | right         ->  T.Right
    | enter         ->  T.Enter --TODO this isnt really a direction...
    | True          ->  T.None
