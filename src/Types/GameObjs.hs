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
   _name :: String --Maybe should also have id?
  ,_position :: (Double,Double)
  ,_scaleFactor :: Float
  ,_display :: Bool
  --TODO refactor to another record (gifInfo)
  ,_currentImg :: FilePath --fixed over time for static images
  ,_gifPath :: Maybe FilePath --for gifs, the top level dir where component images are found
  ,_inMotion   :: Bool
  ,_dir        :: Direction
} deriving (Show,Eq,Generic)


data GameStatus = InProgress
                | GameOver
                | LevelUp
                | ShowInterface Interface
                deriving (Show)

instance Eq GameStatus where
  ShowInterface _ == ShowInterface _ = True
  GameOver == GameOver = True
  LevelUp == LevelUp = True
  InProgress == InProgress = True
  _ == _ = False

instance Hashable Direction
instance Hashable GameObj

data Player = Player {
   _gameObj     :: GameObj
  ,_score      :: Int
  ,_aliveTime  :: Double
} deriving (Show)


data Board = Board {
   _player1   :: Player
  ,_level     :: Level
  ,_objs      :: HashSet GameObj
} deriving (Show)



data GameState = GameState { 
     _board :: Board
   , _status :: GameStatus
   , _gen :: StdGen
   , _images :: ImageMap
   , _highscore :: Int --TODO how can i make this more general (file refrence maybe?)
   }

data Level = Level {
  _num::Int,
  _displayImage ::String,
  _collisionImage :: String} deriving Show

data Interface = Interface {
  displayText :: String,
  interfaceUpdate :: (GameState, GameInput) -> GameState
}

instance Show Interface where
  show = displayText 

makeLenses ''GameState
makeLenses ''Level

makeLenses ''GameObj
makeLenses ''Board
makeLenses ''Player

