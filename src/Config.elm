module Config exposing (..)

import StaticArray.Index as Index


bigCellSize =
    64


midCellSize =
    48


smallCellSize =
    32


powerStrengths level =
    case level |> Index.toInt of
        0 ->
            1

        1 ->
            1

        2 ->
            1

        _ ->
            2
