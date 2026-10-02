{-# LANGUAGE PatternSynonyms #-}

-- | 2D vector toolkit for the physics 'Vec' type.
--
-- Re-exports the single-precision @geomancy@ 'Vec2' surface used throughout
-- apecs-physics, plus the 2D helpers this library used to inherit from
-- @linear@'s @V2@. The helpers replicate the @linear@ semantics exactly,
-- with scalar results in @Float@ to stay consistent with 'Vec2'.
module Apecs.Physics.Vector
  ( -- * Geomancy Vec2
    Vec2
  , vec2
  , withVec2
  , pattern WithVec2

    -- * Vector space operators
  , (^*)
  , (^/)
  , (*^)
  , (^+^)
  , (^-^)
  , dot
  , norm
  , normalize
  , quadrance
  , zeroVector

    -- * 2D helpers
  , perp
  , angle
  , unangle
  , crossZ
  , ex
  , ey
  ) where

import Geomancy

-- | Perpendicular vector, a counterclockwise 90 degree rotation.
--
-- >>> withVec2 (perp (vec2 1 0)) (,)
-- (0.0,1.0)
perp :: Vec2 -> Vec2
perp v = withVec2 v $ \x y -> vec2 (negate y) x
{-# INLINE perp #-}

-- | Unit vector pointing at the given angle (counterclockwise from +X):
-- @'angle' a = 'vec2' (cos a) (sin a)@.
angle :: Float -> Vec2
angle a = vec2 (cos a) (sin a)
{-# INLINE angle #-}

-- | Angle of a vector, counterclockwise from +X, in radians.
--
-- Follows the @linear@ definition; for @x < 0@ the result is in @(pi/2, pi]@.
unangle :: Vec2 -> Float
unangle v = withVec2 v $ \ax ay ->
  let alpha = asin (ay / norm v)
   in if ax < 0
        then pi - alpha
        else alpha
{-# INLINE unangle #-}

-- | The Z-component of the cross product of two vectors in the XY-plane.
--
-- >>> crossZ (vec2 1 0) (vec2 0 1)
-- 1.0
crossZ :: Vec2 -> Vec2 -> Float
crossZ a b = withVec2 a $ \x1 y1 -> withVec2 b $ \x2 y2 -> x1 * y2 - y1 * x2
{-# INLINE crossZ #-}

ex, ey :: Vec2
ex = vec2 1 0
ey = vec2 0 1
