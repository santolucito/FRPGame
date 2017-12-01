{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module InitGameState where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs
import Render.ImageIO

import Control.Lens
import Codec.Picture 
import qualified Data.HashSet as S

import Settings


initialState :: StdGen -> ImageMap -> GameState 
initialState g is = GameState { 
   _board = emptyBoard is
  ,_status = InProgress
  ,_gen = g
  ,_images = is
}

emptyBoard is = Board {
  _player1 = Player {
    _gameObj  = GameObj {
      _name = "player"
     ,_dir      = Left
     ,_inMotion = False
     ,_position = (0,0)
     ,_scaleFactor = 1.6
     ,_currentImg = ""
     ,_gifPath = Just "Player/"
     ,_display = True}
   ,_aliveTime= 0
   ,_score    = 0}
 ,_objs = S.fromList (
           map (uncurry makeCoin) [(-120,25),(120,25),(120,90),(120,-40)] ++
           [makeGhost (130,0)]
          )
 ,_levelName = Level Settings.levelImageSrc
}

makeCoin x y = GameObj {
   _name = "coin"
  ,_position = (x,y)
  ,_scaleFactor = 1
  ,_display = True
  ,_currentImg = "Coin/coin.png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}

makeGhost (x,y) = GameObj {
   _name = "ghost"
  ,_position = (x,y)
  ,_scaleFactor = 0.13
  ,_display = True
  ,_currentImg = "Ghost/ghost.png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}
