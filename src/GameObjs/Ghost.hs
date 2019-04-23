module GameObjs.Ghost where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens

makeGhost (x,y,color) = GameObj {
   _name = "ghost"
  ,_objectId = 0
  ,_position = (x,y)
  ,_scaleFactor = 0.2
  ,_display = True
  ,_currentImg = "Ghost/ghost-"++color++".png"
  ,_collisionImg = "Ghost/outline.png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
  ,_collider = Just ghostCollide
}

ghostCollide o g =
      (set (status) Paused $ 
      (set (interface) (Interface {
            _active = True,
            _displayText = "Dont run into the monsters! \nPress Enter to restart", 
            _interfaceUpdate = restartGame}))
          g,
       o)

restartGame :: (GameState, GameInput) -> GameState
restartGame (gameState,input) = case input of
  Enter -> set status GameOver gameState
  _ -> gameState

