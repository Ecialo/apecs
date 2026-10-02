## [0.5.0]
### Changed
- **Breaking:** switched from `linear` to `geomancy`. `Vec` is now a
  single-precision `Geomancy.Vec2` instead of `V2 Double`; all vector
  components (`Position`, `Velocity`, `Gravity`, `Convex` vertices, ...)
  hold `Vec2`. Scalar quantities (masses, angles, radii) remain `Double`,
  values are converted with `realToFrac` at the chipmunk FFI boundary.
- `Apecs.Physics` no longer re-exports `Linear.V2`; it re-exports the new
  `Apecs.Physics.Vector` module instead, which provides the `geomancy` `Vec2`
  toolkit (`vec2`, `withVec2`, `WithVec2`, `^*`, `dot`, `norm`, ...) plus
  2D helpers replacing the ones previously inherited from `linear`:
  `perp`, `angle`, `unangle`, `crossZ`, `ex`, `ey`.

### Added
- `Apecs.Physics.Vector` module

## [0.4.6]
### Changed
- (#113) Ineffectual custom build removed from Setup.hs

## [0.4.5]
### Changed
- (#78) Chipmunk bumped to `v7.0.3`, fixes the `sysctl.h` deprecation reported in #77

## [0.4.4]
### Changed
- Increased upper bound `apecs` dependency

## [0.4.3]
### Added
- `addPostStepCallback`

### Changed
- Now runs in `MonadIO` rather than `IO`

## [0.4.2]
### Added
- Query `Impulse` for `Constraints`
- Exposed the `CollisionType` Component

### Changed
- Fixed `ConstraintList` to actually yield constraints instead of shapes

## [0.4.1]
### Changed
- Fix Point Query bug, should no longer give memory issues

## [0.4.0]
### Added
- You now have access to colliding shapes in a `Collision`

## [0.3.2]
### Changed
- Fixed links and added changelog to cabal file
- Added version bounds for dependencies
- Expanded haddocks

## [0.3.1]
### Changed
- added `apecs` version bound

## [0.3.0]
### Added
- `ShapeList` and `ConstraintList` components for bodies, that contain a list of entity indices of their shapes and constraints (read-only).
### Changed
- `Shape`s, `Constraint`s, and `CollisionHandler`s now track their original Haskell representations, and can be meaningfully read.
- `Shape` and `Constraint` now only have a single constructor, that explicitly takes an entity argument indicating what entity it belongs to. Previously, the interface suggested that shapes and constraints were properties of bodies, which was wrong.
- Bodies now track their shapes and constraints in /mutable/ stores
### Removed
- The `ShapeBody` component has been removed. You can find out a shapes body by reading the `Shape` component's `ShapeExtend` constructor directly.
