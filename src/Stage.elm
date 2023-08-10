module Stage exposing (..)

import Cell exposing (Cell(..), Connection)
import Config
import Dict exposing (Dict)
import Dir
import Level exposing (Level)
import RelativePos exposing (RelativePos)
import Set exposing (Set)


type alias Stage =
    { grid : Dict ( Int, Int ) Cell
    , gridSize : Int
    , targets : Dict ( Int, Int ) Int
    , origins : Dict ( Int, Int ) Int
    }


type alias SavedStage =
    { connections :
        Dict
            RelativePos
            { from : RelativePos
            , originId : Int
            , path : List RelativePos
            }
    , paths : Dict RelativePos { origins : Set Int }
    , grid : Dict RelativePos Cell
    , gridSize : Int
    , level : Level
    }


fromDict : { gridSize : Int } -> Dict ( Int, Int ) Cell -> Stage
fromDict args dict =
    { grid = dict
    , targets =
        dict
            |> Dict.toList
            |> List.filterMap
                (\( pos, cell ) ->
                    case cell of
                        Target { id } ->
                            Just ( pos, id )

                        _ ->
                            Nothing
                )
            |> Dict.fromList
    , origins =
        dict
            |> Dict.toList
            |> List.filterMap
                (\( pos, cell ) ->
                    case cell of
                        Origin { id } ->
                            Just ( pos, id )

                        _ ->
                            Nothing
                )
            |> Dict.fromList
    , gridSize = args.gridSize
    }


parse : List String -> Stage
parse rows =
    rows
        |> List.indexedMap
            (\y string ->
                string
                    |> String.toList
                    |> List.indexedMap (\x a -> ( ( x, y ), a ))
            )
        |> List.concat
        |> List.foldl
            (\( ( x, y ), char ) out ->
                let
                    pos =
                        ( x - 1, y - 1 )
                in
                case char of
                    '🟥' ->
                        { out
                            | cells = ( pos, Origin { id = out.nextOriginid } ) :: out.cells
                            , nextOriginid = out.nextOriginid + 1
                        }

                    '🔘' ->
                        { out
                            | cells = ( pos, Target { from = Dict.empty, id = out.nextTargetId } ) :: out.cells
                            , nextTargetId = out.nextTargetId + 1
                        }

                    '⬛' ->
                        { out | cells = ( pos, Wall ) :: out.cells }

                    _ ->
                        out
            )
            { cells = [], nextTargetId = 0, nextOriginid = 0 }
        |> .cells
        |> Dict.fromList
        |> fromDict { gridSize = List.length rows - 2 }


computeActiveConnectionsGeneric :
    Dict Int SavedStage
    -> Connection
    -> ( Int, Int )
    -> Stage
    -> Connection
computeActiveConnectionsGeneric levels connection pos stage =
    levels
        |> Dict.get connection.moduleId
        |> Maybe.map
            (\savedStage ->
                let
                    maxPos =
                        { maxPos = savedStage.gridSize }
                in
                savedStage.connections
                    |> Dict.toList
                    |> List.filterMap
                        (\( to, { from } ) ->
                            from
                                |> RelativePos.toDir maxPos
                                |> Dir.rotate connection.rotation
                                |> Dir.addTo pos
                                |> sendsEnergy
                                    { to =
                                        from
                                            |> RelativePos.reverse maxPos
                                            |> RelativePos.rotate maxPos connection.rotation
                                    }
                                    stage
                                |> Maybe.map
                                    (\{ originId } ->
                                        ( to
                                            |> RelativePos.rotate maxPos connection.rotation
                                        , { from = from |> RelativePos.rotate maxPos connection.rotation
                                          , originId = originId
                                          }
                                        )
                                    )
                        )
                    |> Dict.fromList
            )
        |> Maybe.withDefault Dict.empty
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


computeActiveConnectionsLv1 : List RelativePos -> ( ( Int, Int ), Connection ) -> Stage -> Connection
computeActiveConnectionsLv1 neighborsDir ( pos, connection ) stage =
    let
        maxPos =
            { maxPos = stage.gridSize }
    in
    (case neighborsDir of
        [ dir1, dir2 ] ->
            case
                dir1
                    |> RelativePos.toDir { maxPos = stage.gridSize }
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir1 |> RelativePos.reverse maxPos
                        }
                        stage
            of
                Just { originId } ->
                    [ ( dir2, { from = dir1, originId = originId } ) ]

                Nothing ->
                    case
                        dir2
                            |> RelativePos.toDir maxPos
                            |> Dir.addTo pos
                            |> sendsEnergy
                                { to = dir2 |> RelativePos.reverse maxPos
                                }
                                stage
                    of
                        Just { originId } ->
                            [ ( dir1, { from = dir2, originId = originId } ) ]

                        Nothing ->
                            []

        _ ->
            RelativePos.list maxPos
                |> List.filterMap
                    (\fromDir ->
                        fromDir
                            |> RelativePos.toDir maxPos
                            |> Dir.addTo pos
                            |> sendsEnergy
                                { to = fromDir |> RelativePos.reverse maxPos
                                }
                                stage
                            |> Maybe.map
                                (\{ originId } ->
                                    ( fromDir |> RelativePos.reverse maxPos
                                    , { from = fromDir, originId = originId }
                                    )
                                )
                    )
    )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


sendsEnergy :
    { to : RelativePos
    }
    -> Stage
    -> ( Int, Int )
    -> Maybe { originId : Int }
sendsEnergy args stage pos =
    case stage.grid |> Dict.get pos of
        Just (ConnectionCell connection) ->
            connection.sendsTo
                |> Dict.get args.to
                |> Maybe.map (\{ originId } -> { originId = originId })

        Just (Origin { id }) ->
            Just { originId = id }

        _ ->
            Nothing


clear : Level -> Stage -> Stage
clear level stage =
    { stage
        | grid =
            stage.grid
                |> Dict.filter
                    (\( x, y ) _ ->
                        x == -1 || x == stage.gridSize || y == -1 || y == stage.gridSize
                    )
    }
