module Reticule.Types (
  RenderedImage,
  Ray (..),
  Point,
  Color,
  dot,
  distance,
  makeUnitVec,
  unitVec,
  toRGBA8,
  at,
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

at :: Float -> Ray -> Point
at t (Ray origin' direction') = origin' + direction' * pure t

type Point = V3 Float
type Color = V3 Float

toRGBA8 :: Color -> PixelRGBA8
toRGBA8 (V3 r g b) =
  PixelRGBA8
    (floor (r * 255.99))
    (floor (g * 255.99))
    (floor (b * 255.99))
    255

makeUnitVec :: Point -> Point
makeUnitVec vec =
  let k = 1.0 / quadrance vec
   in fmap (* k) vec
{-# INLINE makeUnitVec #-}

unitVec :: Point -> Point
unitVec v = v / pure (quadrance v)
{-# INLINE unitVec #-}
