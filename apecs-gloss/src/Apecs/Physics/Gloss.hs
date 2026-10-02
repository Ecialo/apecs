{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Apecs.Physics.Gloss
  ( convexToPicture
  , Transform
  , worldTransform
  , drawBody
  , simulate
  , module Apecs.Gloss
  ) where

import Control.Monad (foldM)
import Data.Semigroup (Semigroup (..))
import Geomancy (Vec2, pattern WithVec2, withVec2)
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)

import Apecs
import Apecs.Physics

import Apecs.Gloss

convexToPicture :: Convex -> Picture
convexToPicture (Convex [WithVec2 x y] radius) = Translate x y $ Circle (realToFrac radius)
convexToPicture (Convex [a, b] _) = Line [vec2ToTuple a, vec2ToTuple b]
convexToPicture (Convex verts _) = Polygon (map vec2ToTuple verts)

vec2ToTuple :: Vec2 -> (Float, Float)
vec2ToTuple v = withVec2 v (,)

type Transform = (Position, Angle)
worldTransform :: Transform -> Picture -> Picture
worldTransform (Position (WithVec2 x y), Angle theta) =
  Translate x y
    . Rotate (negate . radToDeg . realToFrac $ theta)

drawBody :: (Has w IO Physics) => (Body, Transform, ShapeList) -> System w Picture
drawBody (btype, transform, ShapeList shapes) = color shColor . worldTransform transform <$> foldM foldfn mempty shapes
  where
    foldfn pic shapeEty = do
      Shape _ convex <- get shapeEty
      return . mappend pic $ convexToPicture convex
    shColor = case btype of
      DynamicBody -> red
      KinematicBody -> green
      StaticBody -> blue

simulate
  :: ( Has w IO Camera
     , Has w IO Physics
     )
  => Display
  -> System w ()
simulate disp = do
  w <- ask
  liftIO $
    simulateIO
      disp
      black
      60
      w
      (\_ -> runSystem draw w)
      (\_ _ _ -> runSystem (stepPhysics (1 / 60)) w >> return w)
  where
    draw = do
      pic <- foldDrawM drawBody
      cam <- get global
      return $ cameraTransform cam pic
