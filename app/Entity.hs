module Entity where

data Entity = MkEntity { size      :: Int
                       , position  :: Point
                       , direction :: Vector
                       , speed     :: Float
                       }

data Vector = Vec Float Float
data Point = Pt Float Float