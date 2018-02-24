module Utils where

import Types.Common
import Types.GameObjs

-- | conveniences
showInterface :: GameState -> Bool
showInterface g = _status g == ShowInterface

isGameOver :: GameState -> Bool
isGameOver g = _status g == GameOver

leveledUp :: GameState -> Bool
leveledUp g = _status g == LevelUp
