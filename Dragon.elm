import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias Point = (Float, Float)

type alias Model =
  { points : List Point
  , level : Int
  , frame : Int
  }

maxLevel = 12
frameCount = 100

init : Model
init = { points = [(-200.0, -70.0), (200.0, -70.0)]
       , level = 0 
       , frame = 0
       }

-- New point between two existing points.  Offset to left or right
newPoint : Point -> Point -> Float -> Point
newPoint  (x0,y0) (x1,y1) offset = 
  let (vx, vy) = ((x1 - x0) / 2.0, (y1 - y0) / 2.0) 
      (dx, dy) = (-vy * offset , vx * offset )
  in  (x0 + vx + dx, y0 + vy + dy) --offset from midpoint 

-- Insert between existing points. Offset to left or right side.
newPoints : Float -> List Point -> List Point
newPoints offset points = 
  case points of
    [] -> []
    [p0] -> [p0]  
    p0::p1::rest -> p0 :: newPoint p0 p1 offset :: newPoints -offset (p1::rest)

update : Model -> Model
update model = 
  let nextFrame = model.frame + 1
  in if (nextFrame == frameCount) then
       { points = newPoints 1.0 model.points 
       , level = model.level+1
       , frame = 0
       }
     else
       { model | frame = nextFrame
       }

-- break a list up into n equal sized lists.
breakupInto : Int -> List a -> List (List a)
breakupInto n ls = 
    let segmentCount = (List.length ls) - 1 
        breakup n ls = case ls of
          [] -> []
          _ -> List.take (n+1) ls :: breakup n (List.drop n ls)
    in if n > segmentCount 
       then [ls]
       else breakup (segmentCount // n) ls

view model = 
  let offset = toFloat (model.frame) / toFloat frameCount
      colors = [red, orange, green, blue] 
  in layers
       [ collage 700 500
         (model.points 
           |> newPoints offset
           |> breakupInto (List.length colors) -- for coloring
           |> List.map path 
           |> List.map2 (\color path -> traced (solid color) path ) colors )
         , show model.level
       ]
main =
  Signal.foldp (\_ c -> c+1) 0 (every (5*millisecond))
     |> Signal.filter ((>) (maxLevel*frameCount+1)) 0 
     |> Signal.foldp (\_ d -> update d) init 
     |> Signal.map view

