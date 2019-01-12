{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Input.Input where

import qualified Types.Common as T

import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Yampa (Event(..), SF, returnA, accumHoldBy, arr)

import Data.Maybe

import System.Exit
import System.IO.Unsafe

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event [T.InputEvent]) T.GameInput
parseInput = proc keys -> do
  -- TODO refactor - this is getting so repetitive
  -- Keys that we can hold down
  up    <- accumHoldBy (readKey $ G.SpecialKey G.KeyUp) False    -< keys
  down  <- accumHoldBy (readKey $ G.SpecialKey G.KeyDown) False  -< keys
  left  <- accumHoldBy (readKey $ G.SpecialKey G.KeyLeft) False  -< keys
  right <- accumHoldBy (readKey $ G.SpecialKey G.KeyRight) False -< keys
  -- Keys that are pressed as an event
  enter <- arr (readKeyEvent $ G.SpecialKey G.KeyEnter) -< keys
  space <- arr (readKeyEvent $ G.SpecialKey G.KeySpace) -< keys
  pause <- arr (readKeyEvent $ G.Char 'p')            -< keys
  exit  <- arr (readKeyEvent $ G.SpecialKey G.KeyEsc) -< keys
  returnA -< calcDir (up,down,left,right,enter,space,pause,exit)
 where 
  readKeyEvent k events = case events of
    Event ks -> elem (k, G.Down) $ getK ks
    NoEvent -> False
  readKey k lastVal events = if 
    | elem (k, G.Up)   $ getK events -> False
    | elem (k, G.Down) $ getK events -> True 
    | otherwise                      -> lastVal
  getK = mapMaybe getK'
  getK' (G.EventKey k d _ _ ) = Just (k,d)
  getK' _ = Nothing

  -- Note that ordering matters here
  calcDir (up,down,left,right,enter,space,pause,exit) = if 
     --TODO these arent really directions. maybe GameInput should be of type (Direction,ControlInput)?
    | enter         ->  T.Enter
    | space         ->  T.Space 
    | pause         ->  T.Pause
    -- this exits in a way that cabal profiling can handle
    -- TODO kill the running sclang instance
    | exit          ->  ($!) (\_ -> undefined) (unsafePerformIO exitSuccess) 
    | up && left    ->  T.UpLeft
    | up && right   ->  T.UpRight
    | down && left  ->  T.DownLeft
    | down && right ->  T.DownRight
    | up            ->  T.Up
    | down          ->  T.Down
    | left          ->  T.Left
    | right         ->  T.Right
    | True          ->  T.None
