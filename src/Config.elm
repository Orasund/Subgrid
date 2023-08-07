module Config exposing (..)

import Level
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index


bigCellSize =
    64


midCellSize =
    48


smallCellSize =
    32


maxPos level =
    if level == Index.first then
        1

    else
        gridSize (Level.previous level |> Maybe.withDefault Index.first)


gridSize level =
    if level == Index.first then
        2

    else
        4


powerStrengths level =
    case level |> Index.toInt of
        0 ->
            1

        1 ->
            1

        _ ->
            2
