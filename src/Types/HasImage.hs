module Types.HasImage where

import Types.GameObjs
import Settings

import Control.Lens

import Data.Char
import Numeric

import System.IO.Unsafe --TODO remove these two
import System.Directory

-- | tough to tell where this module stops and ImageIO begins
class HasImageSrc a where
  getImageSrc :: a -> String
 
 -- | Instances for each gameObj we need to render
instance HasImageSrc GameObj where
  getImageSrc o = _currentImg o
 
instance HasImageSrc Types.GameObjs.Level where
  getImageSrc l = _displayImage l


instance HasImageSrc Player where
  getImageSrc p = getImageSrc $ _gameObj p

{-
updatePlayerGif :: Double -> Player -> Player
updatePlayerGif time p =  let
  t = if _inMotion p then time else 0 
  d = _dir p
 in
  set (gameObj.currentImg) (getGifFrame t $ show d) p -}

updateGif :: Double -> GameObj -> GameObj
updateGif time o = let
  t = if _inMotion o then time else 0 
 in
  case _gifPath o of
    Just gPath -> set currentImg (getGifFrame t $ (gPath++(show $ _dir o))) o
    Nothing -> o

--Assume every gif (test.gif) has been expanded to
--test_0.gif, test_1.gif, etc
--this is of course a terrible idea, but it should work
getGifFrame :: Double -> FilePath -> FilePath
getGifFrame time fdir = let
  listOfFiles = unsafePerformIO $ listDirectory (Settings.imageDir ++ fdir) --TODO if this isnt being cached, move somewhere else 
  extension = reverse $ takeWhile (/='.') $ reverse $ head listOfFiles
  -- extract gif delay value
  gifDelay = read ((reverse. takeWhile (\c -> c=='.' || isDigit c). drop (length ("s."++extension)). reverse) $ head listOfFiles) :: Double
  numFrames = length listOfFiles
  thisFrame = (floor (time / gifDelay)) `mod` numFrames
 in
  fdir ++"/frame_" ++ (show thisFrame) ++ "_delay-"++(showFullPrecision gifDelay)++"s."++extension
   
showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

