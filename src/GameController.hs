{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module
    GameController
where

import System.Random (StdGen)
import FRP.Yampa

import Types.Common
import Utils
import Types.GameObjs
import qualified GameLogic
import qualified InterfaceLogic
import InitGameState

import Control.Lens
import Debug.Trace

-- | Run the game while the player ('notDead')
-- when the player ('lostGame'), then ('restartGame')
wholeGame :: StdGen -> ImageMap -> GameState -> SF GameInput GameState
wholeGame g is gs = switch
  (runLevel gs >>> (identity &&& levelEvent))
  (changeLevel g is )

-- | Run the game, keeping the internal state using dHold, updating the
-- game state based on user's input (if any)
-- We could use dHold here, it might be more efficent
-- i think its more clear to use iPre tho and avoid the Event wrapper
runLevel :: GameState -> SF GameInput GameState
runLevel state = proc input -> do
  rec currentState <- iPre state -< updatedState
      updatedState <- update -< (currentState, input)
  returnA -< updatedState --currentState

update :: SF (GameState, GameInput) GameState
update = proc (gs,gi) -> do
  nextGs <- if showInterface gs
            then InterfaceLogic.update -< (gs,gi)
            else arr id -< gs
  nextGs' <- if (_status gs/=Paused)
             then GameLogic.update      -< (nextGs,gi)
             else arr id -< nextGs
  returnA -< nextGs'


levelEvent :: SF GameState (Event GameState)
levelEvent = proc s -> do
  lost <- edge -< isGameOver s
  lUp  <- edge -< leveledUp s
  let lostSnap  = lost `tag` s
  let levelSnap = lUp `tag` s
  returnA -< mergeEvents [lostSnap, levelSnap]

-- | When the game is lost we want to show the GameOver text for some time
-- and then restart the game
changeLevel :: StdGen -> ImageMap -> GameState -> SF GameInput GameState
changeLevel g is gs = case _status gs of
  GameOver -> switch
    (gameOver gs &&& after 5 ())
    (const $ wholeGame g is (set highscore (_highscore gs) $ initialState g is))
  LevelUp -> switch
    (leveling gs &&& after 5 ())
    (const $ wholeGame g is savedLevel) --if you level up, keep some info from last level
  InProgress -> error "Tried to change level while InProgress" 
  Paused -> error "Paused"
 where
  savedLevel = 
    over (board.level) (\l-> Level{_num=1+_num l,_displayImage=_displayImage l,_collisionImage = _collisionImage l}) $
    set status InProgress $
    set (board.player1.gameObj.dir) None $
    set (board.player1.gameObj.position) (view (gameObj.position) initPlayer) $
    set (board.objs) (initObjs ((view (board.level.num) gs)+4)) gs

-- | When we have lost the game we want to keep the board in a state that
-- the user reached and show some GameOver message over it
gameOver :: GameState -> SF a GameState
gameOver s = arr $ const $ s { _status = GameOver }

leveling :: GameState -> SF a GameState
leveling s = arr $ const $ s { _status = LevelUp }

