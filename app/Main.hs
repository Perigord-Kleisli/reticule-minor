module Main (main) where

import Viewport qualified as V
import Reticule.Renderer qualified as R
import Control.Concurrent
import Codec.Picture
import Codec.Picture.Types

main :: IO ()
main = do
  image <- createMutableImage 400 300 (PixelRGBA8 255 255 255 0)
  _ <- forkIO $ R.renderer image
  V.viewWindow image
