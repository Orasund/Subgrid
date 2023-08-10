module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Platform.Cmd as Cmd
import Stage exposing (SavedStage)
import StaticArray.Index as Index
import Time
import View
import View.Dialog


type Dialog
    = LevelComplete
    | LevelSelect
    | TileSelect ( Int, Int )
    | Tutorial (List String)


type alias Model =
    { game : Maybe Game
    , updating : Bool
    , stage : Int
    , level : Level
    , dialog : Maybe Dialog
    , tileSelected : Maybe { moduleId : Int, rotation : Int }
    }


type Msg
    = Toggle ( Int, Int )
    | PlaceModule { moduleId : Int, rotation : Int, pos : ( Int, Int ) }
    | UpdateGrid
    | NextStage
    | LoadStage { level : Level, stage : Int }
    | RemoveTile ( Int, Int )
    | ClearStage
    | SelectLevel
    | SelectTile (Maybe { moduleId : Int, rotation : Int })
    | SetDialog (Maybe Dialog)


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            Index.first

        stage =
            1
    in
    ( { game = Game.Generate.new { level = level, stage = stage } |> Maybe.map Game.fromStage
      , updating = False
      , level = level
      , stage = 1
      , dialog =
            [ "Power all targets (circles) to solve the level."
            , "Connect the power (red squares) with the target by clicking on the tiles between."
            , "Each level has 2-5 possible solutions."
            ]
                |> Tutorial
                |> Just
      , tileSelected = Nothing
      }
    , Cmd.none
    )


loadStage : { stage : Int, level : Level } -> Model -> Maybe Model
loadStage args model =
    model.game
        |> Maybe.andThen
            (\game ->
                game.levels
                    |> Dict.get (Level.toString args.level)
                    |> Maybe.withDefault Dict.empty
                    |> Dict.get args.stage
                    |> Maybe.map Stage.fromSave
                    |> Maybe.map Just
                    |> Maybe.withDefault (Game.Generate.new args)
                    |> Maybe.map
                        (\stage ->
                            { model
                                | game = game |> Game.loadStage stage |> Just
                                , level = args.level
                                , stage = args.stage
                                , dialog = Nothing
                            }
                        )
            )


generateStage : { stage : Int, level : Level } -> Model -> Model
generateStage args model =
    { model
        | game =
            Maybe.map2 Game.loadStage
                (Game.Generate.new args)
                model.game
        , level = args.level
        , stage = args.stage
        , dialog = Nothing
    }


view : Model -> Html Msg
view model =
    [ [ (if
            model.game
                |> Maybe.map (Game.isSolved model.level)
                |> Maybe.withDefault False
         then
            View.primaryButton (SetDialog (Just LevelComplete)) "Done"

         else
            Layout.none
        )
            |> View.topBar
                { level = model.level
                , stage = model.stage
                }
      , View.game []
            { onToggle = \pos -> Just (Toggle pos)
            , level = model.level
            , cellSize = Config.bigCellSize
            }
            model.game
      , model.game
            |> View.tileSelect
                { selected = model.tileSelected
                , editTile = LoadStage
                , unselect = SelectTile Nothing
                , level = model.level
                , selectTile = \a -> SelectTile (Just { moduleId = a.moduleId, rotation = a.rotation })
                , cellSize = Config.smallCellSize
                , clearStage = ClearStage
                }
            |> View.card [ Layout.gap 16 ]
      ]
        |> Layout.column
            [ Layout.gap 16
            , Html.Attributes.style "padding" "1rem"
            , Html.Attributes.style "width" ((Config.bigCellSize * 6 |> String.fromInt) ++ "px")
            ]
    , model.dialog
        |> Maybe.andThen
            (\dialog ->
                case dialog of
                    LevelComplete ->
                        View.Dialog.levelSolved
                            { level = model.level
                            , stage = model.stage
                            , game = model.game
                            , nextStage = NextStage
                            , dismiss = SetDialog Nothing
                            }
                            |> Just

                    LevelSelect ->
                        View.Dialog.levelSelect
                            { load = LoadStage
                            , game = model.game
                            , dismiss = SetDialog Nothing
                            }
                            |> Just

                    TileSelect selected ->
                        model.game
                            |> Maybe.map
                                (View.Dialog.tileSelect
                                    { removeTile = RemoveTile
                                    , selected = selected
                                    , unselect = SetDialog Nothing
                                    , level = model.level
                                    , placeModule = \a -> PlaceModule { moduleId = a.moduleId, rotation = a.rotation, pos = selected }
                                    , cellSize = Config.smallCellSize
                                    }
                                )

                    Tutorial message ->
                        View.Dialog.tutorial
                            { message = message
                            , dismiss = SetDialog Nothing
                            }
                            |> Just
            )
        |> Maybe.map
            (\{ content, dismiss } ->
                content
                    |> View.card
                        Layout.centered
                    |> Layout.el
                        (Layout.centered
                            ++ [ Html.Attributes.style "width" "100%"
                               , Html.Attributes.style "height" "100%"
                               , Html.Attributes.style "position" "absolute"
                               , Html.Attributes.style "backdrop-filter" "blur(2px)"
                               ]
                            ++ Layout.asButton
                                { label = "Dismiss"
                                , onPress = dismiss
                                }
                        )
            )
        |> Maybe.withDefault Layout.none
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=device-width, initial-scale=1"
        ]
        []
    , View.stylesheet
    ]
        |> Html.div
            [ Layout.asEl
            , Html.Attributes.style "position" "relative"
            , Html.Attributes.style "color" Color.fontColor
            , Html.Attributes.style "background" (Color.background model.level)
            , Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "height" "100%"
            , Layout.contentCentered
            ]


placeModule model { moduleId, rotation, pos } =
    model.game
        |> Maybe.map
            (\game ->
                game.stage.grid
                    |> Dict.insert pos
                        ({ moduleId = moduleId
                         , rotation = rotation
                         , sendsTo = Dict.empty
                         }
                            |> ConnectionCell
                        )
                    |> (\grid ->
                            { model
                                | game = Just { game | stage = game.stage |> (\stage -> { stage | grid = grid }) }
                                , updating = True
                                , dialog = Nothing
                                , tileSelected = Nothing
                            }
                       )
            )
        |> Maybe.withDefault model


findNextStage : Model -> Model
findNextStage m =
    let
        model =
            case
                m |> loadStage { level = m.level, stage = m.stage + 1 }
            of
                Just a ->
                    if a.game |> isValid m.level then
                        { m | stage = m.stage + 1 } |> findNextStage

                    else
                        a

                Nothing ->
                    m.level
                        |> Level.next
                        |> Maybe.map
                            (\level ->
                                case m |> loadStage { level = level, stage = 1 } of
                                    Just a ->
                                        if a.game |> isValid level then
                                            { m | level = level, stage = 1 } |> findNextStage

                                        else
                                            a

                                    Nothing ->
                                        { m | game = Nothing }
                            )
                        |> Maybe.withDefault { m | game = Nothing }

        isValid level maybeGame =
            maybeGame
                |> Maybe.map (\game -> game |> Game.update level |> Tuple.first |> Game.isSolved level)
                |> Maybe.withDefault False
    in
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ( x, y ) ->
            let
                gridSize =
                    model.game
                        |> Maybe.map (\{ stage } -> stage.gridSize)
                        |> Maybe.withDefault 1
            in
            ( (if x >= 0 && x < gridSize && y >= 0 && y < gridSize then
                case model.tileSelected of
                    Just a ->
                        placeModule model { moduleId = a.moduleId, rotation = a.rotation, pos = ( x, y ) }

                    Nothing ->
                        model.game
                            |> Maybe.map
                                (\game ->
                                    if model.level == Index.first then
                                        game.stage.grid
                                            |> Dict.update ( x, y )
                                                (\maybe ->
                                                    case maybe of
                                                        Just (ConnectionCell _) ->
                                                            Nothing

                                                        Nothing ->
                                                            Just
                                                                (Dict.empty
                                                                    |> Cell.connectionLevel1
                                                                    |> ConnectionCell
                                                                )

                                                        _ ->
                                                            maybe
                                                )
                                            |> (\grid ->
                                                    { model
                                                        | game =
                                                            { game
                                                                | stage = game.stage |> (\stage -> { stage | grid = grid })
                                                            }
                                                                |> Just
                                                    }
                                               )

                                    else
                                        case game.stage.grid |> Dict.get ( x, y ) of
                                            Just (ConnectionCell _) ->
                                                { model | dialog = TileSelect ( x, y ) |> Just }

                                            Nothing ->
                                                { model | dialog = TileSelect ( x, y ) |> Just }

                                            _ ->
                                                model
                                )
                            |> Maybe.withDefault model

               else
                model
              )
                |> (\m -> { m | updating = True })
            , Cmd.none
            )

        PlaceModule a ->
            ( placeModule model a
            , Cmd.none
            )

        UpdateGrid ->
            let
                ( newGrid, updating ) =
                    model.game
                        |> Maybe.map
                            (\game ->
                                game
                                    |> Game.update model.level
                                    |> Tuple.mapFirst Just
                            )
                        |> Maybe.withDefault ( Nothing, False )
            in
            ( { model
                | game = newGrid
                , updating = updating
                , dialog =
                    if
                        not updating
                            && (newGrid
                                    |> Maybe.map (Game.isSolved model.level)
                                    |> Maybe.withDefault False
                               )
                    then
                        Just LevelComplete

                    else
                        model.dialog
              }
            , Cmd.none
            )

        NextStage ->
            ( model.game
                |> Maybe.map
                    (\game ->
                        game
                            |> Game.saveLevel { level = model.level, stage = model.stage }
                            |> (\g -> { model | game = Just g })
                    )
                |> Maybe.withDefault model
                |> findNextStage
            , Cmd.none
            )

        LoadStage level ->
            ( case model |> loadStage level of
                Just a ->
                    a

                Nothing ->
                    model |> generateStage level
            , Cmd.none
            )

        RemoveTile pos ->
            ( model.game
                |> Maybe.map
                    (\game ->
                        game.stage.grid
                            |> Dict.remove pos
                            |> (\grid ->
                                    { model
                                        | game =
                                            { game
                                                | stage = game.stage |> (\stage -> { stage | grid = grid })
                                            }
                                                |> Just
                                        , dialog = Nothing
                                        , updating = True
                                    }
                               )
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        ClearStage ->
            ( model.game
                |> Maybe.map
                    (\game ->
                        game
                            |> Game.clearStage model.level
                            |> (\g ->
                                    { model
                                        | game = g |> Just
                                        , dialog = Nothing
                                        , updating = True
                                    }
                               )
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        SelectLevel ->
            ( { model | dialog = LevelSelect |> Just }, Cmd.none )

        SetDialog dialog ->
            ( { model | dialog = dialog }, Cmd.none )

        SelectTile tileId ->
            ( { model | tileSelected = tileId }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.updating then
        Time.every 75 (\_ -> UpdateGrid)

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
