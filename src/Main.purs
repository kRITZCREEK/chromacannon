module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Color

import Data.Array (snoc, filter, length)
import Data.Int (toNumber)
import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid (mempty)
import Data.Tuple

import Debug.Trace

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing
import Graphics.Drawing.Font (font, monospace)

import Math (atan2, sin, cos, max)

import Signal hiding (filter)
import Signal.DOM
import Signal.Time

import Unsafe.Coerce

type Vector2D = {x :: Number, y :: Number}

addV2 :: Vector2D -> Vector2D -> Vector2D
addV2 {x: x1, y: y1} {x: x2, y: y2} = {x: x1 + x2, y: y1 + y2}

scaleV2 :: forall e
           . Number
           -> {x :: Number, y :: Number | e}
           -> {x :: Number, y :: Number | e}
scaleV2 s v@({x, y}) = v {x=s*x, y=s*y}

type Projectile =
  { position :: Vector2D
  , velocity :: Vector2D
  , color :: Color
  }

type State =
  { projectiles :: Array Projectile
  , cooldown :: Int
  , cannonDirection :: Number
  }

hole :: forall a. a
hole = unsafeCoerce unit

cannonPosition = {x: 18.0, y: 280.0}

cull :: forall e. {position :: Vector2D | e} -> Boolean
cull {position: {x, y}} = x >= 0.0 && x <= 800.0 && y >= 0.0 && y <= 600.0

move :: forall e
        . {position :: Vector2D, velocity :: Vector2D | e}
        -> {position :: Vector2D, velocity :: Vector2D | e}
move (r@{position, velocity}) = r {position = addV2 position velocity}

step { deltat, click, position: {x, y} } { projectiles, cooldown } =
  let newProjectiles =
      map move >>> filter cull $ projectiles
      cannonDirection = atan2 (negate ((max cannonPosition.x x) - cannonPosition.x)) (y - cannonPosition.y)
  in
   if click && cooldown == 0
   then { projectiles: snoc newProjectiles { position: cannonPosition
                                           , velocity: scaleV2 10.0
                                                { x: negate (sin cannonDirection)
                                                , y: cos cannonDirection
                                                }
                                           , color: colorFromDirection cannonDirection
                                           }
        , cannonDirection
        , cooldown: 10
        }
   else { projectiles: newProjectiles
        , cannonDirection
        , cooldown: if cooldown == 0 then 0 else cooldown - 1
        }

initialState = { projectiles: [], cannonDirection: 0.0, cooldown: 0}

clear = filled (fillColor black) (rectangle 0.0 0.0 800.0 600.0) <> filled (fillColor white) (rectangle 10.0 10.0 780.0 580.0)
clearCannon = translate 18.0 220.0 $ filled (fillColor white) (circle 0.0 0.0 102.5)

colorFromDirection cannonDirection = hsl (360.0 * (sin cannonDirection)) 0.8 0.5

renderS :: State -> Drawing
renderS { projectiles, cannonDirection, cooldown } =
  clear <> shadow (shadowColor black <> shadowBlur 5.0) cannon <> foldMap renderProjectile projectiles <> infos
  where
    infoText = text (font monospace 16 mempty) 100.0 30.0 (fillColor black)
    infos = infoText ("Projectiles: " <> show (length projectiles))
            <> (translate 0.0 20.0 $ infoText ("CannonDirection: " <> show (cannonDirection)))
    color = fillColor (colorFromDirection cannonDirection)
    flash = if cooldown == 0
            then filled (fillColor black) $ mempty
            else filled (fillColor black) (circle 0.0 0.0 (20.0 + 30.0 / (toNumber cooldown)))
                 <> filled (fillColor white) (circle 0.0 0.0 (10.0 + 30.0 / (toNumber cooldown)))
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

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
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
  let state = foldp step initialState (sampleOn tick inputs)
  runSignal $ state ~> render ctx <<< renderS
