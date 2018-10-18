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


-- TODO Need a way to display interface and not pause
updateLamp :: GameObj -> (GameState, GameInput) -> GameState
updateLamp o (gameState,input) = let
  updater newPic = 
    set (interface.active) False $ 
    set status InProgress $ 
    over (board.objs) (S.insert (o{_currentImg = newPic }). S.delete o) gameState
 in
  case input of
    Enter  -> updater "Lamp/lightsOn.png"
    Space -> updater "Lamp/lightsOff.png"
    _ -> gameState

lampCollide :: GameObj -> GameState -> (GameState, GameObj)
lampCollide o g =
      (set (interface) (Interface {
            _active = True,
            _displayText = "Wow, you found a lamp! \nPress Enter to turn on, Space to turn off", 
            _interfaceUpdate = updateLamp o})
          g,
       o)
