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
    "color-mix(in lch,"
        ++ (primaryColors |> StaticArray.get level)
        ++ ", color-mix(in lch, white "
        ++ String.fromInt (originId * 50)
        ++ "%,black) 20% )"


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


primaryColors : StaticArray LevelAmount String
primaryColors =
    ( --yellow
      "#e0e12e"
    , [ --red
        --"#cc353c"
        "#ff6a6a"
      , --green
        "#23bf24"
      , --orange
        "#f4ad36"
      , --türkis
        "#0096c0"
      , --violett
        --"#cc35a1"
        "#ed5ac3"
      ]
    )
        |> StaticArray.fromList Level.maxLevel


secondary : String
secondary =
    --dark blue
    "#122a58"
