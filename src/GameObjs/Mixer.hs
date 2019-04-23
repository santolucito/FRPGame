{-# LANGUAGE MultiWayIf #-}

module GameObjs.Mixer where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens
import qualified Data.HashSet as S


name :: String
name = "mixer"

makeMixer :: Int -> Double -> Double -> GameObj
makeMixer objId x y = GameObj {
   _name = GameObjs.Mixer.name
  ,_objectId = objId
  ,_position = (x,y)
  ,_scaleFactor = 2
  ,_display = True
  ,_currentImg = ""
  ,_collisionImg = "Mixer/Mixer.png"
  ,_gifPath = Just "Mixer/"
  ,_inMotion = True
  ,_dir      = None
  ,_collider = mixerCollide
}

isMixerOn :: Int -> GameState -> Bool
isMixerOn objId g =
  not $ S.null $ S.filter (\o -> _name o == ("mixer") && 
                                 _objectId o == objId && 
                                 _inMotion o == True) $ 
                          (_objs. _board) g

updateMixer :: GameObj -> (GameState, GameInput) -> GameState
updateMixer o (gameState,input) =
  case input of
    Space -> over (board.objs) (S.insert (o{_inMotion = not $ _inMotion o, _currentImg = "Mixer/Mixer.png"}). S.delete o) gameState
    _ -> gameState

mixerCollide :: GameObj -> GameState -> (GameState, GameObj)
mixerCollide mixer g =
      (set (interface) (Interface {
            _active = True,
            _displayText = "Wow, you found a mixer! \nPress Space to turn on/off", 
            _interfaceUpdate = updateMixer mixer})
          g,
       mixer)
