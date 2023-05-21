{-# LANGUAGE TypeFamilies #-}

{- | PartialNum is a typeclass that allows for numerical operations
between 2 different types.
-}
module Data.PartialNum where

import Data.Cast

class (Num a, Num b, Num c) => PartialNum a b c where
  infixl 6 +?
  (+?) :: a -> b -> c
  infixl 6 -?
  (-?) :: a -> b -> c
  infixl 7 *?
  (*?) :: a -> b -> c

infixl 7 /?
class (Num a, Num b, Num c) => PartialFractional a b c where
  (/?) :: a -> b -> c

-- Screw you people without Latex keyboard completion
--
infixl 7 ÷

-- | Unicode alias for `(/?)`
(÷) :: PartialFractional a b c => a -> b -> c
(÷) = (/?)

infixl 7 ×

-- | Unicode alias for `(*?)`
(×) :: PartialNum a b c => a -> b -> c
(×) = (*?)

instance (Num a, Num b, Cast a b) => PartialNum a b b where
  (+?) x y = cast x + y
  (*?) x y = cast x * y
  (-?) x y = cast x - y

instance (Num a, Num b, Cast a b) => PartialNum b a b where
  (+?) x y = x + cast y
  (*?) x y = x * cast y
  (-?) x y = x - cast y

instance (Fractional a, Fractional b, Cast a b) => PartialFractional a b b where
  (/?) x y = cast x / y

instance (Fractional a, Fractional b, Cast a b) => PartialFractional b a b where
  (/?) x y = x / cast y

instance (Num a, Num b, Num c, Fractional c, Cast a c, Cast b c) => PartialFractional a b c where
  (/?) x y = cast x / cast y
