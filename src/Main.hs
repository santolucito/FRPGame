{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Main where

import GameController
import InitGameState 
import Types.Common

import Render.Render 
import Render.GlossInterface
import Render.ImageIO

import Input.Input

import FRP.Yampa (Event(..), SF, (>>>), (<<<), returnA)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Data.Map (union)
import System.Random (newStdGen, StdGen)

import qualified Settings
import GHC.IO.Encoding

main :: IO()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  
  playGame

-- | load a random numbe gen
--   NB : read in every image we will ever need
--   this might use up too much memor if the game uses many images since we have no way to evict an image (I think)
playGame :: IO ()
playGame =do
    g <- newStdGen

    levelImgs <- makeImgMap levelImgSrcs
    playerImgs <- makeImgMap playerImgSrcs
    boardImgs <- boardImgMap
    --coinImg <- makeImgMap coinImgSrc
    
    let imgs = levelImgs `union` playerImgs `union` boardImgs
    playYampa
        (if Settings.fullscreen 
           then G.FullScreen
           --offset window by windoesize so everything is on screen
           else (G.InWindow "Yampa Example" Settings.windowSize Settings.windowSize) )
        G.white
        Settings.fps
        (mainSF g imgs)

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> ImageMap -> SF ((Event InputEvent),(Int,Int)) G.Picture
mainSF g is = proc (e,displaySize) -> do
  updatedGs <- wholeGame g is (initialState g is) <<< parseInput -< e
  pic <- drawGame -< (updatedGs,displaySize)
  returnA -< pic
