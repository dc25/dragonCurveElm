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
init = { points = [(-100.0, 0.0), (100.0, 0.0)]
       , level = 0 
       }

segmentize : List Point -> List (Point, Point)
segmentize points = case points of
    [] -> []
    [p0] -> []
    p0::p1::rest -> (p0, p1) :: segmentize rest
        

next : Dragon -> Dragon
next dragon = 
    let len = toFloat (List.length dragon.points)
    in { points = dragon.points ++ [( len + 0.4, 0.5 )]
       , level = dragon.level+1
       }

main =
  let dragon = Signal.foldp (\_ d -> next d) init (every second)
  in Signal.filter (\d -> d.level < 7) init dragon
     |> Signal.map view 

view dragon = layers
            [ collage 400 400
                (dragonSegments dragon)
              , show "Click to stamp a pentagon."
            ]

dragonSegments : Dragon -> List Form
dragonSegments dragon = 
    dragon.points
    |> segmentize 
    |> List.map (uncurry segment) 
    |> List.map (traced (solid orange)) 
       
