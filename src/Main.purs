module Main where

import Prelude

import Data.Array (snoc, filter, length)
import Data.Int (toNumber)
import Data.Foldable (any, foldMap)
import Data.Maybe
import Data.Monoid (mempty)
import Data.Tuple

import Debug.Trace

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing
import Graphics.Drawing.Font (font, monospace)

import Math (atan2, sin, cos, max)

import Signal as Signal
import Signal.DOM
import Signal.Time (Time, since, every)

import Unsafe.Coerce

import Vector2D as V2
import Vector2D (Vector2D)

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
  }

hole :: forall a. a
hole = unsafeCoerce unit

cannonPosition = {x: 18.0, y: 280.0}

cull :: forall e. {position :: Vector2D | e} -> Boolean
cull {position: {x, y}} = x >= 0.0 && x <= 800.0 && y >= 0.0 && y <= 600.0

newEnemy = {position: {x: 700.0, y: 200.0}, velocity: {x: negate 0.5, y: 2.0 }, color: black, hit: false}

newProjectile cannonDirection =
  { position: cannonPosition
  , velocity: V2.scaleV2 10.0 { x: negate (sin cannonDirection)
                              , y: cos cannonDirection
                              }
  , color: colorFromDirection cannonDirection
  , hit: false
  }

move :: forall e
        . {position :: Vector2D, velocity :: Vector2D | e}
        -> {position :: Vector2D, velocity :: Vector2D | e}
move (r@{position, velocity}) = r {position = V2.addV2 position velocity}

bounce :: Enemy -> Enemy
bounce e@{position: {y}, velocity: {x: vx, y: vy}} =
  let newVelocity = if y < 10.0 || y > 400.0
                    then {x: vx, y: negate vy}
                    else {x: vx, y: vy}
  in e { velocity = newVelocity}

mark :: forall e
        . {projectiles :: Array Projectile, enemies :: Array Enemy | e}
        -> {projectiles :: Array Projectile, enemies :: Array Enemy | e}
mark s@{projectiles, enemies} =
  let collides {position: {x: x1, y: y1 }} {position: {x: x2, y: y2 }} =
        Math.abs (x1 - x2) < 20.0 && Math.abs (y1 - y2) < 20.0
  in
   s { projectiles = (\p -> p {hit = any (collides p) enemies}) <$> projectiles
     , enemies = (\e -> e {hit = any (collides e) projectiles}) <$> enemies
     }

sweep = filter (not <<< _.hit)

step { deltat, click, position: {x, y} } { projectiles, enemies, enemyCooldown, cannonCooldown } =
  let newProjectiles = sweep >>> map move >>> filter cull $ projectiles
                       `append` if doesNewProjectileSpawn
                                then [newProjectile cannonDirection] else []
      newEnemies = (sweep >>> map (bounce >>> move) $ enemies)
                   `append` if enemyCooldown == 0
                            then [newEnemy] else []
      newEnemyCooldown = if enemyCooldown == 0 then 300 else enemyCooldown - 1
      newCannonCooldown = if doesNewProjectileSpawn
                          then 10
                          else if cannonCooldown == 0
                               then 0
                               else cannonCooldown - 1
      cannonDirection = atan2 (negate ((max cannonPosition.x x) - cannonPosition.x)) (y - cannonPosition.y)
      doesNewProjectileSpawn = click && cannonCooldown == 0
  in
   mark { projectiles: newProjectiles
        , enemies: newEnemies
        , enemyCooldown: newEnemyCooldown
        , cannonDirection
        , cannonCooldown: newCannonCooldown
        }

initialState = { projectiles: [], enemies: [], enemyCooldown: 0, cannonDirection: 0.0, cannonCooldown: 0}

clear = filled (fillColor black) (rectangle 0.0 0.0 800.0 600.0) <> filled (fillColor white) (rectangle 10.0 10.0 780.0 580.0)
clearCannon = translate 18.0 220.0 $ filled (fillColor white) (circle 0.0 0.0 102.5)

colorFromDirection cannonDirection = hsl (360.0 * (sin cannonDirection)) 0.8 0.5

renderS :: State -> Drawing
renderS { projectiles, enemies, cannonDirection, cannonCooldown } =
  clear
  <> shadow (shadowColor black <> shadowBlur 5.0) cannon
  <> foldMap renderProjectile projectiles
  <> foldMap renderEnemy enemies
  <> infos
  where
    infoText = text (font monospace 16 mempty) 100.0 30.0 (fillColor black)
    infos = infoText ("Projectiles: " <> show (length projectiles))
            <> (translate 0.0 20.0 $ infoText ("CannonDirection: " <> show (cannonDirection)))
            <> (translate 0.0 40.0 $ infoText ("Enemies: " <> show (length enemies)))
    color = fillColor (colorFromDirection cannonDirection)
    flash = if cannonCooldown == 0
            then filled (fillColor black) $ mempty
            else filled (fillColor black) (circle 0.0 0.0 (20.0 + 30.0 / (toNumber cannonCooldown)))
                 <> filled (fillColor white) (circle 0.0 0.0 (10.0 + 30.0 / (toNumber cannonCooldown)))
    cannon = translate cannonPosition.x cannonPosition.y
             <<< rotate cannonDirection
             $ translate (negate 10.0) 0.0
             (filled color (rectangle 0.0 0.0 20.0 100.0))
             <> translate (negate 10.0) 0.0
             (outlined (outlineColor black <> lineWidth 3.0) (rectangle 0.0 0.0 20.0 100.0))
             <> filled color (circle 0.0 0.0 15.0)
             <> outlined (outlineColor black <> lineWidth 3.0) (circle 0.0 0.0 15.0)
             <> flash

renderProjectile { position: {x, y}, velocity, color } =
  let c = (circle x y 20.0)
  in filled (fillColor color) c <> outlined (lineWidth 1.0) c

renderEnemy { position: {x, y}, velocity, color } =
  let c = (circle x y 30.0)
  in filled (fillColor color) c

main = do
  Just c <- getCanvasElementById "canvas"
  ctx <- getContext2D c

  -- tick <- animationFrame
  let tick = every 16.0
  clicks <- mouseButton 0
  pos <- mousePos
  let inputs = ({deltat: _, click: _, position: _} <$>
                tick <*>
                clicks <*>
                (pos <#> \ {x, y} -> {x: toNumber x, y: toNumber y}))
  let state = Signal.foldp step initialState (Signal.sampleOn tick inputs)
  Signal.runSignal $ state <#> render ctx <<< renderS
