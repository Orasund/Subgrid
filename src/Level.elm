module Level exposing (..)

import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index, Six)
import StaticArray.Length as Length exposing (Length)


type alias LevelAmount =
    Six


type alias Level =
    Index LevelAmount


maxLevel : Length LevelAmount
maxLevel =
    Length.six


previous : Level -> Maybe Level
previous level =
    level |> Index.decrease


next : Level -> Maybe Level
next level =
    level |> Index.increase maxLevel


toString : Level -> String
toString level =
    level |> Index.toInt |> (+) 1 |> String.fromInt


tutorials : StaticArray LevelAmount (List String)
tutorials =
    ( --Level 1
      [ "Connect all targets (circles) to the power supply by clicking on empty tiles."
      ]
    , [ --Level 2
        [ "The solutions to the previous levels become the tiles for the next level."
        ]
      , --Level 3
        [ "Tiles have to be rotated in order to solve the level." ]
      , --Level 4
        [ "Targets with two circles need two power supplies of the same color." ]
      , --Level 5
        [ "Most levels have more than one solution." ]
      , --Level 6
        [ "Find new solutions of previous levels to get better tiles." ]
      ]
    )
        |> StaticArray.fromList maxLevel
