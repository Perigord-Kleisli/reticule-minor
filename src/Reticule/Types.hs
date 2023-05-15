module Reticule.Types where
import Codec.Picture.Types
import Control.Monad.ST

type RenderedImage = MutableImage RealWorld PixelRGBA8
