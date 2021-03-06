{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.GameObjs where

import Types.Common
import Control.Lens (makeLenses)
import System.Random
import Data.HashSet
import Data.Hashable
import GHC.Generics

data GameObj = GameObj {
   _name :: String
  ,_objectId :: Int
  ,_position :: (Double,Double)
  ,_scaleFactor :: Double
  ,_display :: Bool
  --TODO refactor to another record (gifInfo)
  ,_currentImg :: FilePath -- ^ fixed over time for static images
  ,_collisionImg :: FilePath -- ^ the pointer in the image map to the image data that is the set of pixels that are black in the collision img
  ,_gifPath :: Maybe FilePath -- ^ for gifs, the top level dir where component images are found
  ,_inMotion   :: Bool
  ,_dir        :: Direction
  ,_collider   :: GameObj -> GameState -> (GameState, GameObj)
} deriving (Generic)


data GameStatus = InProgress
                | GameOver
                | LevelUp
                | Paused
                deriving (Eq,Show)

instance Hashable Direction
instance Hashable GameObj where
  hashWithSalt n o = hashWithSalt n (
    (_name o) ++
    (show $ _objectId o) ++
    (show $ _position o) ++
    (show $ _currentImg o))

instance Eq GameObj where
  (==) x y = 
    (_name x == _name y) &&
    (_objectId x == _objectId y) &&
    (_position x == _position y) &&
    (_currentImg x == _currentImg y)

data Player = Player {
   _gameObj     :: GameObj
  ,_score      :: Int
  ,_aliveTime  :: Double
} 


data Board = Board {
   _player1   :: Player
  ,_level     :: Level
  ,_objs      :: HashSet GameObj
} 

data GameState = GameState { 
     _board :: Board
   , _status :: GameStatus
   , _interface :: Interface
   , _gen :: StdGen
   , _images :: ImageMap
   , _highscore :: Int --TODO how can i make this more general (file refrence maybe?)
   }

data Level = Level {
  _num::Int,
  _displayImage ::String,
  _collisionImage :: String} deriving Show

data Interface = Interface {
  _active :: Bool,
  _displayText :: String,
  _interfaceUpdate :: (GameState, GameInput) -> GameState
}

instance Show Interface where
  show = _displayText 

makeLenses ''GameState
makeLenses ''Level
makeLenses ''Interface

makeLenses ''GameObj
makeLenses ''Board
makeLenses ''Player

