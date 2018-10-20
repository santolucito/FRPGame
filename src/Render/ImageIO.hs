{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Render.ImageIO where

import Types.Common
import Types.GameObjs
import Types.HasImage
import Settings

import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Juicy
import Codec.Picture

import qualified Data.Map as M --TODO probably should be a hash map?
import Data.Maybe
--import Debug.Trace

-- | Where in the file system do images come from
--   TODO: make this automatically read the dir
--   REALLY TODO: this is so dumb, auto read for directory please
levelImgSrcs :: [FilePath]
levelImgSrcs = map (Settings.imageDir++) ["Coin/coin.png", "Ghost/ghost-orange.png", "Ghost/ghost-purple.png", "Lamp/lightsOn.png", "Lamp/lightsOff.png", "Lamp/lightsOutline.png"]

myReadImage s = do
  i <- readImage s
  return $ either (error ("couldnt read image: "++s)) convertRGBA8 i

boardImgMap :: IO(ImageMap)
boardImgMap = do
 lvlImgPic <- myReadImage (Settings.imageDir++Settings.levelImageSrc)
 lvlImgCollisions <- myReadImage (Settings.imageDir++Settings.levelCollisionImageSrc)
 let pic = fromImageRGBA8$ lvlImgPic
 return $ M.singleton (Settings.imageDir++Settings.levelImageSrc) (lvlImgCollisions, pic)

playerImgSrcs :: [FilePath]
playerImgSrcs = let
  f d = map (\x-> d++"/frame_"++(show x)++"_delay-0.06s.gif") [0..9]
 in
  map ("pics/Player/"++) (concatMap f  ["Right", "Down", "Left", "Up", "UpLeft", "UpRight", "DownLeft", "DownRight", "None"])--TODO ensure all these frames exist

-- | we need different images for differnet character states
--   use a map from state names (string from file name) to image
makeImgMap :: [FilePath] -> IO(ImageMap)
makeImgMap fileNames = do
 allImages <- mapM readImage fileNames --TODO use myReadImage
 let imgs = map (either blackImage convertRGBA8) allImages -- the image data
 let pics = map fromImageRGBA8 imgs  -- the pic to display
 let toMap ks vs = M.fromList$ zip ks vs
 return $ toMap fileNames (zip imgs pics)

 -- | get the chacter state image given a player state
 --   we also simulate a gif here (TODO: do we tho?)
getImg :: HasImageSrc a => GameState -> a -> (Image PixelRGBA8,G.Picture)
getImg g o = let
  s = getImageSrc o
  allImgs =  _images g
  myImg = M.lookup (Settings.imageDir ++ s) allImgs
 in
  fromMaybe (error (("Could not find image : "++ (show (Settings.imageDir++s)))++" \nWithin available image paths : "++show (M.keys allImgs))) myImg

  
