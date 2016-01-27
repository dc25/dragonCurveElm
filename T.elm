import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias Point = (Float, Float)

type alias Dragon =
    { points : List Point
    , level : Int
    }

init : Dragon
init = { points = [(-200.0, 0.0), (200.0, 0.0)]
       , level = 0 
       }

type Side = Left | Right

otherSide : Side -> Side
otherSide s = 
    case s of 
        Left -> Right
        Right -> Left
        

bend : Point -> Point -> Side -> Point
bend  (x0,y0) (x1,y1) s = 
    let (vx, vy) = ((x1 - x0) / 2.0, (y1 - y0) / 2.0)
        (mx, my) = (x0 + vx, y0 + vy)
        (dx, dy) = case s of 
                      Left -> (-vy, vx)
                      Right -> (vy, -vx)
    in  (mx + dx, my + dy)

evolve : List Point -> Side -> List Point
evolve points side = case points of
    [] -> []
    [p0] -> [p0]  
    p0::p1::rest -> p0 :: bend p0 p1 side :: evolve (p1::rest) (otherSide side)

next : Dragon -> Dragon
next dragon = 
    { points = evolve dragon.points Left
    , level = dragon.level+1
    }

main =
  let dragon = Signal.foldp (\_ d -> next d) init (every (100*millisecond))
  in Signal.filter (\d -> d.level < 14) init dragon
     |> Signal.map view 

view dragon = layers
            [ collage 700 700 
              [ dragon.points |> path |> traced (solid orange) ]
              , show (toString dragon.level)
            ]
