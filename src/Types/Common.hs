{-# LANGUAGE DeriveGeneric #-}

module Types.Common where

--import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import GHC.Generics

import qualified Data.Map as M
import Codec.Picture

-- | A map between image file names to ...
-- | the Image (fst) is used for look at the data of the picture
-- | the G.Picutre is used for rendering
type ImageMap = M.Map String (Image PixelRGBA8,G.Picture)

blackPixel = PixelRGB8 0 0 0
blackAPixel = PixelRGBA8 0 0 0 255
whitePixel = PixelRGB8 255 255 255
blackImage = (\_-> generateImage (\_ _ -> blackAPixel) 10 10)

--TODO should i separate movement and interface controls?
data Direction = 
  Up | Down | Left | Right |
  UpLeft | UpRight | DownLeft | DownRight | 
  Enter | None 
  deriving (Eq, Enum, Bounded, Show, Generic)

nextDir :: Direction -> Direction
nextDir d = if d==maxBound then minBound else succ d

prevDir :: Direction -> Direction
prevDir d = if d==minBound then maxBound else pred d

data GameStatus = InProgress
                | GameOver
                | LevelUp
                | ShowInterface String
                deriving (Eq, Show)


type GameInput = Direction
--type GameInput = Event Direction
type InputEvent = G.Event

