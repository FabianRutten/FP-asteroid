module Entity where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

data Entity = MkEntity { size      :: Int
                       , position  :: Point
                       , direction :: Vector 
                       , speed     :: Float --pixels p/tick (30/s)
                       }

--data Vector = Vec Float Float
--data Point = Pt Float Float