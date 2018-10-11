module Utils where

import Types.Common
import Types.GameObjs

import Data.Maybe 

-- | conveniences
showInterface :: GameState -> Bool
showInterface = isJust . getInterfaceText

getInterfaceText :: GameState -> Maybe String
getInterfaceText g = case _status g of
  ShowInterface t -> Just $ displayText t
  _ -> Nothing

isGameOver :: GameState -> Bool
isGameOver g = _status g == GameOver

leveledUp :: GameState -> Bool
leveledUp g = _status g == LevelUp
