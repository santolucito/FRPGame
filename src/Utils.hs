module Utils where

import Types.GameObjs

-- | conveniences
showInterface :: GameState -> Bool
showInterface = _active. _interface

getInterfaceText :: GameState -> String
getInterfaceText = _displayText. _interface

isGameOver :: GameState -> Bool
isGameOver g = _status g == GameOver

leveledUp :: GameState -> Bool
leveledUp g = _status g == LevelUp

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)
