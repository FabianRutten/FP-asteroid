-- | This module contains the data types
--   which represent the state of the game
module Model where

import Entity

--initials
initialSpace :: Space
initialSpace = MkSpace initialPlayer [] [] [] False (replicate 3 False)

initialPlayer :: Player
initialPlayer = MkPlayer (MkEntity 1 (Pt 400 400) (Vec 0 0) 0) (Vec 0 0) 3 0

data Space = MkSpace { player        :: Player
                     , asteroids     :: [Asteroid]
                     , saucers       :: [Saucer]
                     , bullets       :: [Bullet]
                     , paused        :: Bool
                     , arrowkeysDown :: [Bool]
                     }

type Saucer = Entity
type Asteroid = Entity

data Bullet = MkBullet { projectile :: Entity
                       , fromPlayer :: Bool
                       , distance   :: Float
                       }



data Player = MkPlayer { ship        :: Entity
                       , orientation :: Vector
                       , lives       :: Int
                       , score       :: Int
                       }

