{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Main where

import GameController
import InitGameState 
import Types.Common

import Render.Render 
import Render.GlossInterface
import Render.ImageIO

import Render.Audio
import qualified Sound.Tidal.Context as T
import System.Process

import Input.Input

import FRP.Yampa (Event(..), SF, (<<<), returnA, arr)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Data.Map (union)
import System.Random (newStdGen, StdGen)

import qualified Settings
import GHC.IO.Encoding
import System.Exit
import System.Environment
import Control.Monad

main :: IO()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  
  a <- getArgs
  -- pass any command line arg in to enable audio
  scPID <- if (length a > 0) then spawnCommand "sclang boot.scd" >>= (return . Just) else return Nothing
  playGame $ scPID

-- | load a random numbe gen
--   NB : read in every image we will ever need
--   this might use up too much memor if the game uses many images since we have no way to evict an image (I think)
playGame :: Maybe ProcessHandle -> IO ()
playGame scPID = do

    g <- newStdGen

    levelImgs <- makeImgMap levelImgSrcs
    playerImgs <- makeImgMap playerImgSrcs
    boardImgs <- boardImgMap
    
    let imgs = levelImgs `union` playerImgs `union` boardImgs
    playYampa
        (if Settings.fullscreen 
           then G.FullScreen
           --offset window by windowsize so everything is on screen
           else (G.InWindow "FRP Game" Settings.windowSize Settings.windowSize) )
        G.white
        Settings.fps
        (mainSF g imgs)
        scPID
    die "ended the program for testing"

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> ImageMap -> SF (Event [InputEvent],(Int,Int)) (G.Picture, T.Pattern T.ControlMap, Bool)
mainSF g is = proc (es,displaySize) -> do
  inputs <- parseInput -< es
  updatedGs <- wholeGame g is (initialState g is) -< inputs
  pic <- arr drawGame -< (updatedGs,displaySize)
  newTidalPattern <- arr calculateAudio -< updatedGs
  returnA -< (pic, newTidalPattern, inputs == Exit)

