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
init = { points = [(-200.0, 0.0), (200.0, 0.0)]
       , level = 0 
       , frame = 0
       }

type Side = Left | Right

otherSide : Side -> Side
otherSide side = 
    case side of 
        Left -> Right
        Right -> Left

newPoint : Point -> Point -> Side -> Float -> Point
newPoint  (x0,y0) (x1,y1) side progress = 
    let (vx, vy) = ((x1 - x0) / 2.0, (y1 - y0) / 2.0) 
        (mx, my) = (x0 + vx, y0 + vy)  -- midpoint
        (dx, dy) = case side of 
                      Left -> (-vy * progress , vx * progress )
                      Right -> (vy * progress , -vx * progress )
    in  (mx + dx, my + dy) --offset from midpoint to left or right 

-- Insert between existing points. Offset to left or right side.
newPoints' : Side -> Float -> List Point -> List Point
newPoints' side progress points = 
  case points of
    [] -> []
    [p0] -> [p0]  
    p0::p1::rest -> p0 :: newPoint p0 p1 side progress :: newPoints' (otherSide side) progress (p1::rest)

newPoints : Float -> List Point -> List Point
newPoints progress points = newPoints' Left progress points

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

view model = 
    let progress = (toFloat (model.frame)/toFloat frameCount)
    in layers
          [ collage 700 700 
            [ model.points 
                |> newPoints progress
                |> path 
                |> traced (solid purple) 
            ]
            , show model.level
          ]
main =
  Signal.foldp (\_ c -> c+1) 0 (every (5*millisecond))
     |> Signal.filter (\c -> c < maxLevel*frameCount+1) 0 
     |> Signal.foldp (\_ d -> update d) init 
     |> Signal.map view

