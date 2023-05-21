{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reticule.Utils where

import Control.Lens
import Language.Haskell.TH.Quote
import Linear.V2
import Numeric (readHex)

scalar :: Applicative f => a -> f a
scalar = pure

fromTup :: (a, a) -> V2 a
fromTup = uncurry V2

toTup :: V2 a -> (a, a)
toTup (V2 x y) = (x, y)

unV2 :: (a -> a -> b) -> V2 a -> b
unV2 f (V2 x y) = f x y

rgb :: QuasiQuoter
rgb =
  QuasiQuoter
    { quoteExp = \case
        ['#', r1, r2, g1, g2, b1, b2, a1, a2] -> do
          r <- readHex' [r1, r2]
          g <- readHex' [g1, g2]
          b <- readHex' [b1, b2]
          a <- readHex' [a1, a2]
          [|makeColorI r g b a|]
        ['#', r1, r2, g1, g2, b1, b2] -> do
          r <- readHex' [r1, r2]
          g <- readHex' [g1, g2]
          b <- readHex' [b1, b2]
          [|makeColorI r g b 255|]
        x -> fail $ "Parse error with pattern: " ++ x
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
 where
  readHex' =
    maybe (fail "Invalid Hex") (pure . fromInteger @Int)
      . (^? ix 0 . _1)
      . readHex
