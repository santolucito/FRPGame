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
           map (uncurry makeCoin) coinPos ++
           (map makeGhost $ [(130,0,"orange"),(-130,100,"orange"),(-130,20,"purple")])
          )
 ,_levelName = Level Settings.levelImageSrc
}

coinPos =[
  (-120,100),(-120,25),(-120,-50),
  (120,100),(120,25),(120,-50),
  (-60,-50),(0,-50),(60,-50),
  (-60,100),(0,100),(60,100)]
  ++  
  [(x,y) | x <- [-200,200], y <- [-116,-65..340]]
makeCoin x y = GameObj {
   _name = "coin"
  ,_position = (x,y)
  ,_scaleFactor = 0.8
  ,_display = True
  ,_currentImg = "Coin/coin.png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}

makeGhost (x,y,color) = GameObj {
   _name = "ghost"
  ,_position = (x,y)
  ,_scaleFactor = 0.15
  ,_display = True
  ,_currentImg = "Ghost/ghost-"++color++".png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}
