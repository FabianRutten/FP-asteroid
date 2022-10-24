module Entity where

data Entity = MkEntity { size      :: Int
                       , position  :: Point
                       , direction :: Vector
                       , speed     :: Float
                       }

data Vector = MkVector Float Float
data Point = MkPoint Float Float

