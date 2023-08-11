module Color exposing (..)

import Level exposing (Level, LevelAmount)
import StaticArray exposing (StaticArray)


background : Level -> String
background level =
    "linear-gradient(" ++ white ++ ", " ++ inactiveLaser level ++ " 66%)"


tileBackground : String
tileBackground =
    "color-mix(in lch," ++ wallColor ++ ", " ++ white ++ " 20%)"


fontColor : String
fontColor =
    black


wallColor : String
wallColor =
    black


laserColor : Level -> Int -> String
laserColor level originId =
    let
        { primary, trinary } =
            laserColors |> StaticArray.get level
    in
    case originId of
        0 ->
            primary

        1 ->
            "color-mix(in lch, color-mix(in lch," ++ primary ++ ", " ++ trinary ++ " 50%), black 30%)"

        2 ->
            "color-mix(in lch,color-mix(in lch," ++ primary ++ ", " ++ trinary ++ " 75%), white 30%)"

        _ ->
            "color-mix(in lch," ++ primary ++ ", " ++ trinary ++ ")"


inactiveLaser : Level -> String
inactiveLaser level =
    "color-mix(in lch," ++ laserColor level 0 ++ " 33%,white)"



----------------------------------------------------------------------
--
----------------------------------------------------------------------


black : String
black =
    secondary


white : String
white =
    "#f6feff"


lightGray : String
lightGray =
    "#f2f3f3"


darkGray : String
darkGray =
    "#d9d9d9"


laserColors : StaticArray LevelAmount { primary : String, trinary : String }
laserColors =
    let
        yellow =
            --yellow
            "#e0e12e"

        red =
            --red
            --"#cc353c"
            "#ff6a6a"

        green =
            --green
            "#23bf24"

        orange =
            --orange
            "#f4ad36"

        turcoise =
            --türkis
            "#0096c0"

        violet =
            --violett
            --"#cc35a1"
            "#ed5ac3"
    in
    ( { primary = yellow, trinary = orange }
    , [ { primary = green, trinary = turcoise }
      , { primary = turcoise, trinary = yellow }
      , { primary = orange, trinary = red }
      , { primary = red, trinary = violet }
      , { primary = violet, trinary = turcoise }
      ]
    )
        |> StaticArray.fromList Level.maxLevel


secondary : String
secondary =
    --dark blue
    "#122a58"
