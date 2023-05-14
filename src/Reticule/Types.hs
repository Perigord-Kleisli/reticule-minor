module Reticule.Types where
import Codec.Picture.Types
import Control.Monad.ST
import Data.IORef

type RenderedImage = MutableImage RealWorld PixelRGBA8
