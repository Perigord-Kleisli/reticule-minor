module Reticule.Types (
  RenderedImage,
  Ray (..),
  Point,
  dot,
  distance,
  makeUnitVec,
  unitVec,
  toRGBA8,
) where

import Codec.Picture.Types
import Control.Monad.ST
import Linear.Metric
import Linear.V3

-- | The mutable image type utilized both by the renderer and preview window
type RenderedImage = MutableImage RealWorld PixelRGBA8

data Ray = Ray
  { origin :: !Point
  , direction :: !Point
  }
  deriving (Show, Eq, Ord)

type Point = V3 Float

toRGBA8 :: Point -> PixelRGBA8
toRGBA8 (V3 r g b) = PixelRGBA8 (floor (r * 255.99)) 
                                (floor (g * 255.99))
                                (floor (b * 255.99))
                                255

makeUnitVec :: Point -> Point
makeUnitVec vec =
  let k = 1.0 / quadrance vec
   in fmap (* k) vec
{-# INLINE makeUnitVec #-}

unitVec :: Point -> Point
unitVec v = v / toVec3 (quadrance v)
 where
  toVec3 x = V3 x x x
{-# INLINE unitVec #-}
