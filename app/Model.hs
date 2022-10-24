-- | This module contains the data types
--   which represent the state of the game
module Model where

import Entity

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0


data Space = MkSpace { player    :: Player
                     , asteroids :: [Asteroid]
                     , saucers   :: [Saucer]
                     , bullets   :: [Bullet]
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

