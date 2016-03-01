module Vector2D where

import Prelude ((*), (+))

type Vector2D = {x :: Number, y :: Number}

addV2 :: Vector2D -> Vector2D -> Vector2D
addV2 {x: x1, y: y1} {x: x2, y: y2} = {x: x1 + x2, y: y1 + y2}

scaleV2 :: forall e
           . Number
           -> {x :: Number, y :: Number | e}
           -> {x :: Number, y :: Number | e}
scaleV2 s v@({x, y}) = v {x=s*x, y=s*y}
