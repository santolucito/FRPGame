{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Render.Audio where

import Sound.Tidal.Context 
import Sound.Tidal.ParseBP

import Types.GameObjs
import GameObjs.Mixer
import Utils

calculateAudio :: GameState -> Pattern ControlMap
calculateAudio gs = do 
  let numBds = show $ (1+ (_score._player1._board) gs)
  let p1 = sometimes brak $ sound $ parseBP_E $ ("[~ SynDr:1*"++numBds++"] [~ SynDr:8]")
  let p2 = sound "SynDr:4*2" # vowel "e o"
  let p3 = degradeBy 0.7 $ sound "SynDr:7*8" # gain 0.7
  if 
    | isGameOver gs -> silence
    | _status gs == Paused  -> (stack [p1,p2]) # lpf "200"
    | isMixerOn 1 gs -> stack [p1,p2,p3]
    | True -> stack [p1,p2]
