module Game.Generate exposing (..)

import Game exposing (Game)
import Level exposing (Level, LevelAmount)
import Stage
import StaticArray exposing (StaticArray)


levels : StaticArray LevelAmount (Int -> Maybe Game)
levels =
    ( level1
    , [ level2
      , level3
      , level4
      , level7
      , level7
      ]
    )
        |> StaticArray.fromList Level.maxLevel


level1 : Int -> Maybe Game
level1 stage =
    case stage of
        1 ->
            Stage.parse
                [ "⬛🟥⬛"
                , "🔘⬜⬛"
                , "⬛⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            Stage.parse
                [ "⬛🟥⬛"
                , "🔘⬜🟥"
                , "⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


level2 : Int -> Maybe Game
level2 stage =
    case stage of
        1 ->
            Stage.parse
                [ "⬛⬛🟥⬛"
                , "🔘⬜⬜⬛"
                , "⬛⬜⬜⬛"
                , "⬛⬛⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            Stage.parse
                [ "⬛⬛🟥⬛"
                , "🔘⬜⬜🟥"
                , "⬛⬜⬜⬛"
                , "⬛⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


level3 : Int -> Maybe Game
level3 stage =
    case stage of
        1 ->
            Stage.parse
                [ "⬛⬛🟥⬛"
                , "🔘⬜⬜⬛"
                , "⬛⬜⬜🟥"
                , "⬛⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            Stage.parse
                [ "⬛⬛🟥⬛"
                , "🔘⬜⬜🟥"
                , "⬛⬜⬜⬛"
                , "⬛⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


level4 : Int -> Maybe Game
level4 stage =
    case stage of
        1 ->
            Stage.parse
                [ "⬛🔘⬛🟥⬛"
                , "🟥⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬛"
                , "⬛🟥⬛⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            Stage.parse
                [ "⬛🟥⬛🔘⬛"
                , "🔘⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬛"
                , "🟥⬜⬜⬜🔘"
                , "⬛⬛⬛🟥⬛"
                ]
                |> Game.fromStage
                |> Just

        3 ->
            Stage.parse
                [ "⬛🔘⬛🟥⬛"
                , "🔘⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬛"
                , "🟥⬜⬜⬜🟥"
                , "⬛⬛⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


{-| We have to start with these three stages, because you always need three varients in order to solve all levels

An alternative to this would be to introduce a stage that is just a crossing. But this is not really fun.

So this is the next best thing.

-}
level5 : Int -> Maybe Game
level5 stage =
    case stage of
        1 ->
            --player is forced to cross lines
            Stage.parse
                [ "⬛🔘⬛⬛🟥⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛🟥⬛⬛⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            --player learns that lasers will prefer straight lines
            Stage.parse
                [ "⬛🟥⬛⬛🔘⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬛⬛⬛🟥⬛"
                ]
                |> Game.fromStage
                |> Just

        3 ->
            --player will not be able to make a straight line
            Stage.parse
                [ "⬛🔘⬛⬛🟥⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🟥"
                , "⬛⬛⬛⬛🔘⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


level7 stage =
    case stage of
        1 ->
            Stage.parse
                [ "⬛⬛🟥🟥⬛⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜🟥"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬛🔘⬛⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        2 ->
            Stage.parse
                [ "⬛⬛🟥🟥⬛⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬛⬛🔘⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        3 ->
            Stage.parse
                [ "⬛⬛🟥🔘⬛⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬛⬛🔘⬛⬛"
                ]
                |> Game.fromStage
                |> Just

        _ ->
            Nothing


new : { level : Level, stage : Int } -> Maybe Game
new args =
    levels
        |> StaticArray.get args.level
        |> (\fun -> fun args.stage)
