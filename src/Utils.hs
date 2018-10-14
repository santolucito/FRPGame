module Utils where

import Types.Common
import Types.GameObjs

import Data.Maybe 

-- | conveniences
showInterface :: GameState -> Bool
showInterface = _active. _interface

getInterfaceText :: GameState -> String
getInterfaceText = _displayText. _interface

isGameOver :: GameState -> Bool
isGameOver g = _status g == GameOver

leveledUp :: GameState -> Bool
leveledUp g = _status g == LevelUp
