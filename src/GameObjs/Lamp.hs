{-# LANGUAGE MultiWayIf #-}

module GameObjs.Lamp where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens
import qualified Data.HashSet as S


name :: String
name = "lamp"

makeLamp :: Int -> Double -> Double -> GameObj
makeLamp objId x y = GameObj {
   _name = GameObjs.Lamp.name
  ,_objectId = objId
  ,_position = (x,y)
  ,_scaleFactor = 2
  ,_display = True
  ,_currentImg = "Lamp/lightsOn.png"
  ,_collisionImg = "Lamp/lightsOutline.png"
  ,_gifPath = Just "Mixer/"
  ,_inMotion = True
  ,_dir      = None
  ,_collider = lampCollide
}

isLampOn :: Int -> GameState -> Bool
isLampOn objId g =
  not $ S.null $ S.filter (\o -> _name o == ("lamp") && 
                                 _objectId o == objId && 
                                 _currentImg o == "Lamp/lightsOn.png") $ 
                          (_objs. _board) g

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
