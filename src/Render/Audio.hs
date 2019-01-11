{-# LANGUAGE OverloadedStrings #-}

module Render.Audio where

import Sound.Tidal.Context 
import Sound.Tidal.ParseBP

import Types.GameObjs
import GameObjs.Lamp

calculateAudio :: GameState -> Pattern ControlMap
calculateAudio gs = do 
  let numBds = show $ (1+ (_score._player1._board) gs)
  let p1 = sometimes brak $ sound $ parseBP_E $ ("[~ ArabPK:0*"++numBds++"] [~ ArabPK:4]")
  let p2 = sound "ArabPK:1*2" # vowel "e o"
  if isLampOn 1 gs
  then stack $ [p1,p2]
  else p2
