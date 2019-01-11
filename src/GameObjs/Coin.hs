module GameObjs.Coin where

import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import Control.Lens

makeCoin x y = GameObj {
   _name = "coin"
  ,_objectId = 0
  ,_position = (x,y)
  ,_scaleFactor = 0.8
  ,_display = True
  ,_currentImg = "Coin/coin.png"
  ,_collisionImg = ""
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}

coinCollide o g = 
      (over (board.player1.score) (+1) g,
      set display False o)
