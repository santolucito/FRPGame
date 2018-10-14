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
  ,_interface = Interface {_active=False,_displayText="",_interfaceUpdate=undefined}
  ,_gen = g
  ,_images = is
}

initPlayer = Player {
  _gameObj  = GameObj {
    _name = "player"
    ,_dir      = None
    ,_inMotion = False
    ,_position = (-100,50)
    ,_scaleFactor = 1.6
    ,_currentImg = ""
    ,_gifPath = Just "Player/"
    ,_display = True}
  ,_aliveTime= 0
  ,_score    = 0}

emptyBoard is = Board {
  _player1 = initPlayer
 ,_objs = initObjs 3
 ,_level = Level {_num=1,_displayImage=Settings.levelImageSrc,_collisionImage=Settings.levelCollisionImageSrc}
}

initObjs n = S.fromList (
   --map (uncurry makeCoin) coinPos
   [(makeLamp 100 200)] ++
   (map makeGhost $ take n [
        (130,300,"orange"),
        (-250,80,"orange"),
        (130,220,"purple"),
        (-130,20,"purple"),
        (-130,80,"purple"),
        (131,40,"orange"),
        (-139,20,"purple"),
        (145,20,"orange")--TODO, gnerate rather than hard code
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

makeLamp x y = GameObj {
   _name = "lamp"
  ,_position = (x,y)
  ,_scaleFactor = 0.2
  ,_display = True
  ,_currentImg = "Lamp/lightsOn.png"
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
