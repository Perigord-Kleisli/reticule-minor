{-# LANGUAGE OverloadedRecordDot #-}

module Reticule.Renderer where

import Codec.Picture
import Codec.Picture.Types (MutableImage (..))
import Control.Lens ((^.))
import Control.Monad
import Data.PartialNum
import Linear.V3
import Reticule.Types
import Reticule.Utils (scalar)

renderer :: RenderedImage -> IO ()
renderer image = do
  let width = mutableImageWidth image
      height = mutableImageHeight image
      width' = fromIntegral width
      height' = fromIntegral height
      (xRatio, yRatio) =
        if width' > height'
          then (width' / height', 1)
          else (1, height' / width')
      origin = V3 0 0 0
      horizontal = V3 (xRatio * 2) 0 0
      vertical = V3 0 (yRatio * 2) 0
      lower_left = V3 (-xRatio) (-yRatio) (-1)

  forM_ [0 .. width - 1] \x -> forM_ [0 .. height - 1] \y -> do
    let
      u = x ÷ width
      v = y ÷ height

      ray =
        Ray
          origin
          (lower_left
            + scalar u
              * horizontal
            + scalar v
              * vertical)

    writePixel
      image
      x
      ((height - 1) - y)
      (toRGBA8 $ color ray)
 where
  hitSphere center radius ray =
    let
      oc = origin ray - center
      a = dot ray.direction ray.direction
      b = 2 * dot oc ray.direction
      c = dot oc oc - radius ** 2
      descriminant = b ** 2 - 4 * a * c
     in
      if (b ** 2 - 4 * a * c) > 0
        then Just $ (-b - sqrt descriminant) / 2.0 * a
        else Nothing

  color ray = case hitSphere sphereXYZ sphereRadius ray of
    Just t -> makeUnitVec ((at t ray - sphereXYZ) + 1)
    Nothing ->
      let t = (makeUnitVec (direction ray) ^. _y + 1.0) / 2
       in (1.0 - scalar t) * V3 1 1 1 + scalar t * V3 0.5 0.7 1.0
   where
    sphereXYZ = V3 0 0 (-1)
    sphereRadius = 0.5
