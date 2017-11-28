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
      _position = (0,0)
     ,_scaleFactor = 1.6
     ,_currentImg = ""
     ,_gifPath = Just ""
     ,_display = True}
   ,_dir      = Left
   ,_aliveTime= 0
   ,_score    = 0
   ,_inMotion = False}
 ,_objs = S.fromList $ map (uncurry makeCoin) [(-120,25),(120,25),(120,90),(120,-40)]
 ,_levelName = Level Settings.levelImageSrc
}

makeCoin x y = GameObj {
  _position = (x,y)
  ,_scaleFactor = 1
  ,_currentImg = "coin.png"
  ,_gifPath = Nothing
  ,_display = True
}

