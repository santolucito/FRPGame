{-# LANGUAGE OverloadedStrings #-}

module Render.Audio where

import qualified Sound.Tidal.Context as T
import qualified Sound.Tidal.ParseBP as T

import Types.GameObjs

calculateAudio :: GameState -> T.Pattern T.ControlMap
calculateAudio gs = do 
  let numBds = show $ (_score._player1._board) gs
  T.sound $ T.parseBP_E $ ("[bd*"++numBds++"] sn*2")
