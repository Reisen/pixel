module Pixel.HKD ( HKD ) where

import Protolude

--------------------------------------------------------------------------------

-- Higher-Kinded Data (HKD), or Functor Functors is a way of modeling data as a
-- collection of Functor data:
--
-- ```
--   data Example' f = Example
--     { _exampleField1 :: HKD f Text
--     , _exampleField2 :: HKD f Int
--     }
-- ```
--
-- This allows defining ad-hoc structures on the fly by substituting the `f`
-- for specific purposes, a validatable Example:
--
-- ```
--   type UnvalidatedExample = Example' Maybe
--   type Example            = Example' Identity
--
--   validateExample :: UnvalidatedExample -> Example
-- ```
--
-- In order to not have to constantly unwrap/re-wrap Identity when we want to
-- work with just normal data, we use a type-family called HKD which erases the
-- Identity case, giving us our original data back.

type family HKD (f :: * -> *) a where
  HKD Identity a = a
  HKD f        a = f a
