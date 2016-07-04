module Chroma.Render where

import Prelude
import Graphics.Drawing (Drawing, Color, Point, fillColor, filled, circle, shadowBlur, black, shadowColor, shadow, lineWidth, outlineColor, outlined, rectangle, translate, rotate, white, text, hsl)
import Color as Color
import Color.Scheme.MaterialDesign as MD
import Chroma.Types (Enemy, Projectile, State)
import Data.Array (length)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Graphics.Drawing.Font (font, monospace)
import Math (sin)

cannonPosition :: Point
cannonPosition = { x: 18.0, y: 280.0 }

clear :: Drawing
clear = filled (fillColor black) (rectangle 0.0 0.0 800.0 600.0) <> filled (fillColor white) (rectangle 10.0 10.0 780.0 580.0)

colorFromDirection :: Number -> Color
colorFromDirection cannonDirection =
  hsl (360.0 * (sin cannonDirection)) 0.8 0.5

renderS :: State -> Drawing
renderS { lost: true, points } =
  let text' = text (font monospace 60 mempty)
  in
   text' 130.0 280.0 (fillColor MD.red) "AT LEAST YOU TRIED"
   <> text' 130.0 330.0 (fillColor MD.purple) ("POINTS: " <> show points)
renderS { projectiles, enemies, cannonDirection, cannonCooldown, cannonColor, points } =
  clear
  <> shadow (shadowColor black <> shadowBlur 5.0) cannon
  <> foldMap renderProjectile projectiles
  <> foldMap renderEnemy enemies
  <> infos
  where
    infoText = text (font monospace 16 mempty) 100.0 30.0 (fillColor black)
    infos = infoText ("Projectiles: " <> show (length projectiles))
            <> (translate 0.0 20.0 $ infoText ("Points: " <> show (points)))
            <> (translate 0.0 40.0 $ infoText ("Enemies: " <> show (length enemies)))
    color = fillColor cannonColor
    flash = if cannonCooldown == 0
            then filled (fillColor black) mempty
            else filled (fillColor black) (circle 0.0 0.0 (50.0 - toNumber cannonCooldown))
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

renderProjectile :: Projectile -> Drawing
renderProjectile { position: {x, y}, velocity, color } =
  let c = circle x y 20.0
      c1 = filled
             (fillColor (Color.lighten 0.2 color))
             (circle (x - velocity.x) (y - velocity.y) 20.0)
      c2 = filled
             (fillColor (Color.lighten 0.3 color))
             (circle (x - (1.5 * velocity.x)) (y - (1.5 * velocity.y)) 20.0)
  in c2 <> c1 <> filled (fillColor color) c

renderEnemy :: Enemy -> Drawing
renderEnemy { position: {x, y}, velocity, color } =
  let c = circle x y 30.0
  in filled (fillColor color) c
