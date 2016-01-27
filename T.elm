import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias Point =
    { x : Float
    , y : Float
    }

type alias Dragon =
    { points : List Point
    }

init : Dragon
init = {points = [{x=-1.0, y=0.0},{x=1.0, y=0.0}] }

next : Dragon -> Dragon
next dragon = 
    let len = toFloat (List.length dragon.points)
    in { points = dragon.points ++ [{ x = len + 0.4, y= 0.5 }] }

main =
  let countedClock = Signal.foldp (\t (count,dragon, _) -> (count+1,next dragon,  t)) (0,init, 0) (every second)
      filteredClock = Signal.filter (\(c,_,_) -> c < 7) (0,init, 0) countedClock
      uncountedClock = Signal.map (\(_,_,t) -> t) filteredClock
  in Signal.map clock uncountedClock


clock t = layers
            [ collage 400 400
                [ hand orange 100 t
                ]
              , show "Click to stamp a pentagon."
            ]

hand clr len time =
  let
    angle = degrees (90 - 6 * inSeconds time)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)
