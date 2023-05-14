{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Viewport (viewWindow) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Graphics.Gloss
import Data.IORef
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy
import Linear.V2
import Reticule.Utils
import Reticule.Types
import Codec.Picture.Types
import System.Mem
import Control.Monad.Trans.Class


data ViewportState = ViewPortState
  { _lastEvent :: Event
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
  eventText <- Color [rgb|#FFFFFF|] . scale 0.1 0.1 . Text . show . _lastEvent <$> get

  viewportLoc' <- gets _viewportLoc
  viewportScale' <- gets _viewportScale
  image <- gets _renderedImage >>= unsafeFreezeImage
  let viewport =
        Color [rgb|#323232|] $
          unV2 translate viewportLoc' $
            join scale viewportScale' $
              fromImageRGBA8 image
  return (Pictures [viewport, eventText])

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
    (fmap fst . runStateT viewportRenderer)
    (\event -> fmap snd . runStateT (eventHandler event))
    (\t -> fmap snd . runStateT (timeHandler t))
