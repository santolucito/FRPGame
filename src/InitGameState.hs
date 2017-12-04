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
import Debug.Trace


initialState :: StdGen -> ImageMap -> GameState 
initialState g is = GameState { 
   _board = emptyBoard is
  ,_status = InProgress
  ,_gen = g
  ,_images = is
}

initPlayer = Player {
  _gameObj  = GameObj {
    _name = "player"
    ,_dir      = Left
    ,_inMotion = False
    ,_position = (0,25)
    ,_scaleFactor = 1.6
    ,_currentImg = ""
    ,_gifPath = Just "Player/"
    ,_display = True}
  ,_aliveTime= 0
  ,_score    = 0}

emptyBoard is = Board {
  _player1 = initPlayer
 ,_objs = initObjs
 ,_level = Level {num=1,datapath=Settings.levelImageSrc}
}

initObjs = S.fromList (
   map (uncurry makeCoin) coinPos
   ++ (map makeGhost $ [
	(130,0,"orange"),
	(-130,100,"orange"),
	(-130,20,"purple"),
	(130,20,"purple")
	])
  )

coinPos =
     filter (\c-> c/=(40,25) && c/=(-40,25) && 
                  c/=(338,98) && c/=(280,98) &&
                  c/=(-338,98) && c/=(-280,98) &&
                  c/=(338,-48) && c/=(280,-48) &&
                  c/=(-338,-48) && c/=(-280,-48))
     $  line 338 ++ line 280 ++ line 200 ++ line 120 ++ line 40
 where
  line x = [(x',y) | x' <- [-x,x]  , y <- [-bottom,-bottom+(coinSpace)..top]]
  coinSpace = 73
  top = 280
  bottom = 340
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
  ,_scaleFactor = 0.2
  ,_display = True
  ,_currentImg = "Ghost/ghost-"++color++".png"
  ,_gifPath = Nothing
  ,_inMotion = False
  ,_dir      = Left
}
