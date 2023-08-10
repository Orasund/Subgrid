module View.Dialog exposing (..)

import Cell exposing (Cell(..))
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import RelativePos
import Set
import Stage exposing (SavedStage)
import StaticArray.Index as Index
import View
import View.Render
import View.Svg


type alias DialogHtml msg =
    { content : List (Html msg)
    , dismiss : Maybe msg
    }


tileSelect :
    { removeTile : ( Int, Int ) -> msg
    , selected : ( Int, Int )
    , unselect : msg
    , level : Level
    , placeModule : { moduleId : Int, rotation : Int } -> msg
    , cellSize : Int
    }
    -> Game
    -> DialogHtml msg
tileSelect args game =
    [ "Select Tile" |> View.cardTitle
    , "Select a tile you want to place" |> Layout.text []
    , args.level
        |> Level.previous
        |> Maybe.andThen (\level -> game.levels |> Dict.get (Level.toString level))
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.map
            (\( id, level ) ->
                List.range 0 3
                    |> List.map
                        (\rotate ->
                            level.grid
                                |> View.Svg.tile
                                    { tileSize = Config.bigCellSize
                                    , active =
                                        \pos ->
                                            level.paths
                                                |> Dict.get (RelativePos.fromTuple pos)
                                                |> Maybe.map .origins
                                                |> Maybe.withDefault Set.empty
                                                |> Set.toList
                                                |> List.head
                                                |> (\originId -> { originId = originId })
                                    , render = \_ -> View.Render.boxRender
                                    , level = args.level
                                    , background = Color.tileBackground
                                    , cellToColor =
                                        Cell.toColor
                                            { level = Level.previous args.level |> Maybe.withDefault Index.first
                                            }
                                    , gridSize = level.gridSize
                                    }
                                |> Layout.el
                                    [ Html.Attributes.style "transform"
                                        ("rotate(" ++ String.fromInt (rotate * 90) ++ "deg)")
                                    ]
                                |> Layout.el
                                    (Layout.asButton
                                        { label = "Level " ++ String.fromInt id ++ "(" ++ String.fromInt rotate ++ ")"
                                        , onPress = Just (args.placeModule { moduleId = id, rotation = rotate })
                                        }
                                    )
                        )
                    |> Layout.row [ Layout.gap 8 ]
            )
        |> Layout.column [ Layout.gap 8 ]
    , (if
        game.stage.grid
            |> Dict.member args.selected
       then
        [ View.button (args.removeTile args.selected) "Remove" ]

       else
        []
      )
        ++ [ View.primaryButton args.unselect "Cancel" ]
        |> Layout.row [ Layout.gap 8 ]
    ]
        |> (\list -> { content = list, dismiss = Just args.unselect })


levelSelect :
    { load : { level : Level, stage : Int } -> msg
    , game : Maybe Game
    , dismiss : msg
    }
    -> DialogHtml msg
levelSelect args =
    [ "Edit Levels" |> View.cardTitle
    , Index.range Level.maxLevel
        |> List.reverse
        |> List.map
            (\level ->
                args.game
                    |> Maybe.map .levels
                    |> Maybe.andThen (Dict.get (level |> Level.toString))
                    |> Maybe.withDefault Dict.empty
                    |> View.savedLevels { level = level }
                        (\stage ->
                            args.load
                                { level = level
                                , stage = stage
                                }
                        )
            )
        |> Layout.column [ Layout.gap 8 ]
    ]
        |> (\list -> { content = list, dismiss = Just args.dismiss })


levelSolved :
    { level : Level
    , game : Maybe Game
    , stage : Int
    , nextStage : msg
    , dismiss : msg
    }
    -> DialogHtml msg
levelSolved args =
    [ View.stageName { level = args.level, stage = args.stage }
        ++ " Solved"
        |> View.cardTitle
    , View.game []
        { onToggle = \_ -> Nothing
        , level = args.level
        , cellSize = Config.midCellSize
        }
        args.game
    , [ View.button args.dismiss "Dismiss"
      , View.primaryButton args.nextStage "Next Level"
      ]
        |> Layout.row [ Layout.contentWithSpaceBetween ]
    ]
        |> (\list -> { content = list, dismiss = Just args.dismiss })


tutorial : { message : List String, dismiss : msg } -> DialogHtml msg
tutorial args =
    [ "Hint" |> View.cardTitle
    , args.message |> List.map (Layout.text []) |> Layout.column [ Layout.gap 8 ]
    , View.primaryButton args.dismiss "Got it"
    ]
        |> (\list -> { content = list, dismiss = Nothing })
