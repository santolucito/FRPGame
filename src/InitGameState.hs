{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module InitGameState where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs

import qualified Data.HashSet as S

import GameObjs.Lamp
import GameObjs.Coin
import GameObjs.Ghost

import Settings
import Debug.Trace


initialState :: StdGen -> ImageMap -> GameState 
initialState g is = GameState { 
   _board = emptyBoard is
  ,_status = InProgress
  ,_interface = Interface {_active=False,_displayText="",_interfaceUpdate=undefined}
  ,_gen = g
  ,_images = is
  ,_highscore = 0
}

initPlayer = Player {
  _gameObj  = GameObj {
    _name = "player"
    ,_dir      = None
    ,_inMotion = False
    ,_position = (-100,50)
    ,_scaleFactor = 1.6
    ,_currentImg = ""
    ,_collisionImg = "Player/outline.gif"
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
        ]) ++
    [makeCoin 0 100, makeCoin 0 200]
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
