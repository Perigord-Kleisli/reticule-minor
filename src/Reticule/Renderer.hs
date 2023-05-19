module Reticule.Renderer where

import Codec.Picture
import Control.Monad
import Reticule.Types

width :: Int
width = 399

height :: Int
height = 299

renderer :: RenderedImage -> IO ()
renderer image = do
  forM_ [0 .. width] \x -> forM_ [0 .. height] \y -> do
    let
      r :: Float
      r = fromIntegral x / fromIntegral width
      g :: Float
      g = fromIntegral y / fromIntegral height
      b :: Float
      b = 0.2
    writePixel
      image
      x
      (299 - y)
      ( PixelRGBA8
          (floor (r * 255.99))
          (floor (g * 255.99))
          (floor (b * 255.99))
          255
      )
