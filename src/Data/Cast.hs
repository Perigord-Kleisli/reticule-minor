-- Mostly lossless casts, note that "lossless" in this context
-- only covers types with significant overlap, where a majority
-- of the cases have lossless conversions

module Data.Cast where

class Cast a b where
  cast :: a -> b

{- | Never use non-higher-kinded constraints in Cast instances or you will
have countless instnace overlap issues
-}
instance Cast Int Float where
  cast = fromIntegral

instance (Cast a b, Functor f) => Cast (f a) (f b) where
  cast = fmap cast
