{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Common 
  (module Types.Image,
   module Types.Common)
   where

--import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import GHC.Generics

import Types.Image

--TODO should i separate movement and interface controls?
data Direction = 
  Up | Down | Left | Right |
  UpLeft | UpRight | DownLeft | DownRight | 
  Enter | Space | Pause | None 
  deriving (Eq, Enum, Bounded, Show, Generic)

nextDir :: Direction -> Direction
nextDir d = if d==maxBound then minBound else succ d

prevDir :: Direction -> Direction
prevDir d = if d==minBound then maxBound else pred d

type GameInput = Direction
--type GameInput = Event Direction
type InputEvent = G.Event
