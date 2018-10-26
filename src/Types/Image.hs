module Types.Image where

import qualified Data.HashSet as S
import qualified Graphics.Gloss.Interface.IO.Game as G
import Codec.Picture

import qualified Data.Map as M


{- TODO this should be something like
data ImageData = GameImageData ... | CollisionImageData ...
-}

data ImageData = ImageData {
    glossImage :: G.Picture -- ^ the G.Picutre is used for rendering
  , pixelData :: Image PixelRGBA8 -- ^ the Image _ is used for look at the data of the picture
  , blackPixelLocs :: S.HashSet (Int,Int) -- ^ for collision detection, we only need to know where the collision points are
  } deriving (Eq)

-- | A map between image file names to the Image Data so we can keep IO up front
type ImageMap = M.Map String ImageData 

whitePixel, blackPixel :: PixelRGB8
blackPixel = PixelRGB8 0 0 0
whitePixel = PixelRGB8 255 255 255

blackAPixel :: PixelRGBA8
blackAPixel = PixelRGBA8 0 0 0 255
blackImage = (\_-> generateImage (\_ _ -> blackAPixel) 10 10)



