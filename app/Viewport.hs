{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Viewport (viewWindow) where

import Codec.Picture.Types
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Vector.Storable (unsafeToForeignPtr)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact
import Linear.V2
import Reticule.Types
import Reticule.Utils
import System.Exit

data ViewportState = ViewPortState
  { _lastEvent :: !Event
  , _viewportScale :: !Float
  , _viewportOrigin :: V2 Float
  , _viewportLoc :: V2 Float
  , _repeatActions :: [(Event -> Bool, StateT ViewportState IO ())]
  -- ^ Actions that get repeated until (Event -> Bool) returns True
  , _renderedImage :: !RenderedImage
  }

makeLenses ''ViewportState

type Viewport a = StateT ViewportState IO a

initialViewPortState :: RenderedImage -> ViewportState
initialViewPortState image =
  ViewPortState
    { _lastEvent = EventResize (0, 0) -- Sentinel value
    , _viewportScale = 1
    , _viewportOrigin = 0
    , _viewportLoc = 0
    , _repeatActions = []
    , _renderedImage = image
    }

viewportRenderer :: Viewport Picture
viewportRenderer = do
  image <- gets _renderedImage >>= unsafeFreezeImage
  let info =
        translate 0 (negate $ (+ 25) $ (/ 2) $ fromIntegral $ imageHeight image)
          . Color [rgb|#FFFFFF|]
          . scale 0.1 0.1
          . Text
  viewportLoc' <- gets _viewportLoc
  viewportScale' <- gets _viewportScale
  let viewport =
        Color [rgb|#323232|] $
          unV2 translate viewportLoc' $
            join scale viewportScale' $
              fromImageRGBA8 image
  return (Pictures [viewport, info "Rendering"])

fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 (Image{imageWidth = w, imageHeight = h, imageData = imgData}) = bitmapOfForeignPtr w h (BitmapFormat TopToBottom PxRGBA) ptr False
 where
  (ptr, _, _) = unsafeToForeignPtr imgData

untilEvent :: (Event -> Bool) -> Viewport () -> Viewport ()
untilEvent f m = repeatActions %= ((f, m) :)

untilKeyRelease :: Key -> Viewport () -> Viewport ()
untilKeyRelease key = untilEvent \case
  (EventKey key' Up _ _) | key == key' -> True
  _ -> False

eventHandler :: Event -> Viewport ()
eventHandler event = do
  repeatActions %= filter (^. _1 . to (not . ($ event)))
  -- Filter out any repeated event witha key that matches the current event
  lastEvent .= event
  -- To allow the following actions to get the current event if their path
  -- isn't pattern matched. Mostly intended for the `repeatedActions` item

  case event of
    EventKey (SpecialKey KeyEsc) _ _ _ -> lift exitSuccess
    EventKey (MouseButton LeftButton) Down _ clickLoc -> do
      initialCoord <- gets _viewportLoc
      untilKeyRelease (MouseButton LeftButton) do
        gets _lastEvent >>= \case
          EventMotion coord -> do
            viewportLoc .= (fromTup coord - fromTup clickLoc) + initialCoord
          _ -> return ()
    EventKey (MouseButton WheelUp) Up _ (fromTup -> coord) -> do
      viewportOrigin .= coord
      viewportScale *= 1.1
      viewportLoc %= \loc -> ((loc - coord) * 1.1) + coord
    EventKey (MouseButton WheelDown) Up _ (fromTup -> coord) -> do
      viewportOrigin .= coord
      viewportScale %= (/ 1.1)
      viewportLoc %= \loc -> ((loc - coord) / 1.1) + coord
    _ -> return ()

timeHandler :: Float -> Viewport ()
timeHandler _ = do
  gets _repeatActions >>= mapM_ snd

viewWindow :: RenderedImage -> IO ()
viewWindow !image = do
  playIO
    (InWindow "Reticule-Minor viewport" (400, 300) (100, 100))
    [rgb|#0B0B0B|]
    60
    (initialViewPortState image)
    (evalStateT viewportRenderer)
    (execStateT . eventHandler)
    (execStateT . timeHandler)
