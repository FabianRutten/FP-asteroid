module Bitmap where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


giveShipFile :: FilePath
giveShipFile = "/img/ship.bmp"

giveShipPicture :: Picture
giveShipPicture = loadBMP giveShipFile

