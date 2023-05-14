module Main (main) where

import Viewport qualified as V
import Control.Monad
import Control.Concurrent
import Codec.Picture
import Codec.Picture.Types
import Reticule.Types
import Data.IORef

renderer :: RenderedImage -> IO ()
renderer image = do
  forM_ [0..399] \x -> forM_ [0..299] \y -> do
    let r = floor @Float $ (fromIntegral x / 399) * 255
        g = floor @Float $ (fromIntegral y / 299) * 255
    writePixel image x y (PixelRGBA8 r g 255 255)
    threadDelay 10

main :: IO ()
main = do
  image <- createMutableImage 400 300 (PixelRGBA8 255 255 255 0) >>= newIORef
  _ <- forkIO $ renderer image
  V.viewWindow image
