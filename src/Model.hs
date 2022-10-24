-- | This module contains the data types
--   which represent the state of the game
module Model where

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
                     , bullets   :: [Bulet]
                     }

data Entity = MkEntity { size     :: Int
                      , position  :: Point
                      , direction :: Vector
                      , speed     :: Float
                      }

type Saucer = Entity
type Asteroid = Entity

data Bullet = MkBullet { projectile :: Entity
                       , fromPlayer :: Bool
                       , distance   :: Floatd
                       }

data Vector = MkVector Float Float

                