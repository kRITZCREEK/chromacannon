module Main where

import Graphics.Drawing
import Signal as Signal
import Vector2D as V2
import Chroma.Render (renderS, cannonPosition)
import Chroma.Types (Input, State, Projectile, Enemy)
import Control.Monad.Eff (runPure, Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Timer (TIMER)
import DOM (DOM)
import Data.Array (filter, length)
import Data.Foldable (any)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)
import Math (atan2, sin, cos, max, abs)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, const, bind, negate, append, map, not, (<<<), (<#>), (<*>), (<$>), (+), (<=), (||), (==), (&&), (-), (>>>), ($), (<), (>), (>=))
import Signal.DOM (animationFrame, mouseButton, mousePos)
import Signal.Time (every)
import Vector2D (Vector2D)

cull :: forall e. {position :: Vector2D | e} -> Boolean
cull { position: { x, y } } = x >= 0.0 && x <= 800.0 && y >= 0.0 && y <= 600.0

newEnemy :: Color -> Enemy
newEnemy c =
  { position: { x: 700.0, y: 200.0 }
  , velocity: { x: negate 1.0, y: 4.0 }
  , color: c, hit: false
  }

newProjectile :: Number -> Color -> Projectile
newProjectile cannonDirection cannonColor =
  let s = runPure playSound
  in
  { position: cannonPosition
  , velocity: V2.scaleV2 20.0 { x: negate (sin cannonDirection)
                              , y: cos cannonDirection
                              }
  , color: cannonColor
  , hit: false
  }

move :: forall e
        .  { position :: Vector2D, velocity :: Vector2D | e }
        -> { position :: Vector2D, velocity :: Vector2D | e }
move r@{ position, velocity } = r { position = V2.addV2 position velocity }

bounce :: Enemy -> Enemy
bounce e@{position: { y }, velocity: {x: vx, y: vy}} =
  let newVelocity = if y < 30.0 || y > 570.0
                    then {x: vx, y: negate vy}
                    else {x: vx, y: vy}
  in e { velocity = newVelocity}

mark :: forall e
        . {projectiles :: Array Projectile, enemies :: Array Enemy | e}
        -> {projectiles :: Array Projectile, enemies :: Array Enemy | e}
mark s@{projectiles, enemies} =
  let collides {position: {x: x1, y: y1 }, color: c1} {position: {x: x2, y: y2 }, color: c2} =
        abs (x1 - x2) < 20.0 && abs (y1 - y2) < 20.0 && distance c1 c2 < 50.0
  in
   s { projectiles = (\p -> p {hit = any (collides p) enemies}) <$> projectiles
     , enemies = (\e -> e {hit = any (collides e) projectiles}) <$> enemies
     }

sweep :: forall t8. Array { hit :: Boolean | t8} -> Array { hit :: Boolean | t8}
sweep = filter (not <<< _.hit)

step :: Input -> State -> State
step _ s@{lost: true} = s
step { deltat, click, position: {x, y}, randomColor, cannonColor }
     { projectiles, enemies, enemyCooldown, cannonCooldown, points, lost } =
  let newProjectiles = sweep >>> map move >>> filter cull $ projectiles
                       `append` if doesNewProjectileSpawn
                                then [newProjectile cannonDirection cannonColor] else []
      newEnemies = sweep >>> map (bounce >>> move) $ enemies
                   `append` if enemyCooldown == 0
                            then [newEnemy randomColor] else []
      newEnemyCooldown = if enemyCooldown == 0 then 300 else enemyCooldown - 1
      newCannonCooldown = if doesNewProjectileSpawn
                          then 30
                          else if cannonCooldown == 0
                               then 0
                               else cannonCooldown - 1
      newPoints = points + length (filter _.hit enemies)
      cannonDirection = atan2 (negate ((max cannonPosition.x x) - cannonPosition.x)) (y - cannonPosition.y)
      doesNewProjectileSpawn = click && cannonCooldown == 0
      newLost = lost || any (\e -> e.position.x <= 0.0) newEnemies
  in
   mark { projectiles: newProjectiles
        , enemies: newEnemies
        , enemyCooldown: newEnemyCooldown
        , cannonDirection
        , cannonCooldown: newCannonCooldown
        , cannonColor
        , points: newPoints
        , lost: newLost
        }

initialState :: State
initialState = { projectiles: []
               , enemies: []
               , enemyCooldown: 0
               , cannonDirection: 0.0
               , cannonCooldown: 0
               , cannonColor: white
               , points: 0
               , lost: false
               }

foreign import playSound :: forall eff. Eff eff Unit

main :: forall e. Eff ( canvas :: CANVAS
                      , dom :: DOM
                      , random :: RANDOM
                      , timer :: TIMER
                      | e) Unit
main = do
  c <- getCanvasElementById "canvas"
  ctx <- getContext2D (unsafePartial fromJust c)

  tick <- animationFrame
  let tickColor = every 500.0
      cannonColor = _.color <$>
        Signal.foldp
          (\_ {counter} -> {color: hsl counter 0.5 0.5, counter: counter + 10.0})
          {color: white, counter: 0.0}
          (every 100.0)
  clicks <- mouseButton 0
  pos <- mousePos
  randomColor <- Signal.unwrap (const ((\x -> hsl x 0.5 0.5) <$> randomRange 0.0 360.0) <$> tickColor)


  let inputs = {deltat: _, click: _, position: _, randomColor: _, cannonColor: _} <$>
                tick <*>
                clicks <*>
                (pos <#> \ {x, y} -> {x: toNumber x, y: toNumber y}) <*>
                randomColor <*>
                cannonColoj
  let state = Signal.foldp step initialState (Signal.sampleOn tick inputs)
  Signal.runSignal (state <#> render ctx <<< renderS)
