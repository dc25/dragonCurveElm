import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


main =
  let countedClock = Signal.foldp (\t (c,_) -> (c+1,t)) (0,0) (every second)
      filteredClock = Signal.filter (\(c,_) -> c < 7) (0,0) countedClock
      uncountedClock = Signal.map snd filteredClock
  in Signal.map clock uncountedClock


clock t = layers
            [ collage 400 400
                [ filled lightGrey (ngon 12 110)
                , outlined (solid grey) (ngon 12 110)
                , hand orange 100 t
                , hand charcoal 100 (t/60)
                , hand charcoal 60 (t/720)
                ]
              , show "Click to stamp a pentagon."
            ]

hand clr len time =
  let
    angle = degrees (90 - 6 * inSeconds time)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)
