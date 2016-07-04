module Chroma.Types where

import Color (Color)
import Graphics.Drawing (Point)
import Vector2D (Vector2D)

type Input =
  { cannonColor :: Color
  , randomColor :: Color
  , position :: Point
  , click :: Boolean
  , deltat :: Number
  }

type Projectile =
  { position :: Vector2D
  , velocity :: Vector2D
  , color :: Color
  , hit :: Boolean
  }

type Enemy =
  { position :: Vector2D
  , velocity :: Vector2D
  , color :: Color
  , hit :: Boolean
  }

type State =
  { projectiles :: Array Projectile
  , enemies :: Array Enemy
  , enemyCooldown :: Int
  , cannonCooldown :: Int
  , cannonDirection :: Number
  , cannonColor :: Color
  , points :: Int
  , lost :: Boolean
  }
