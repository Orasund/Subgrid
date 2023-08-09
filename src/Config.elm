module Config exposing (..)

import StaticArray.Index as Index


bigCellSize =
    64


midCellSize =
    48


smallCellSize =
    32


gridSize level =
    case level |> Index.toInt of
        0 ->
            1

        1 ->
            2

        2 ->
            2

        3 ->
            3

        _ ->
            4


powerStrengths level =
    case level |> Index.toInt of
        0 ->
            1

        1 ->
            1

        2 ->
            1

        3 ->
            1

        _ ->
            2
