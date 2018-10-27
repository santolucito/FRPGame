{-# LANGUAGE MultiWayIf #-}

module GameObjs.Lamp where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens
import qualified Data.HashSet as S


name :: String
name = "lamp"

makeLamp :: Double -> Double -> GameObj
makeLamp x y = GameObj {
   _name = GameObjs.Lamp.name
  ,_position = (x,y)
  ,_scaleFactor = 0.2
  ,_display = True
  ,_currentImg = "Lamp/lightsOn.png"
  ,_collisionImg = "Lamp/lightsOutline.png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}


updateLamp :: GameObj -> (GameState, GameInput) -> GameState
updateLamp o (gameState,input) = let
  newPic = if
    | _currentImg o == "Lamp/lightsOn.png"  -> "Lamp/lightsOff.png"
    | _currentImg o == "Lamp/lightsOff.png" -> "Lamp/lightsOn.png"
 in
  case input of
    Space -> over (board.objs) (S.insert (o{_currentImg = newPic }). S.delete o) gameState
    _ -> gameState

lampCollide :: GameObj -> GameState -> (GameState, GameObj)
lampCollide lamp g =
      (set (interface) (Interface {
            _active = True,
            _displayText = "Wow, you found a lamp! \nPress Space to turn on/off", 
            _interfaceUpdate = updateLamp lamp})
          g,
       lamp)
