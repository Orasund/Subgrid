module Game exposing (..)

import Cell exposing (Cell(..), Connection)
import Config
import Dict exposing (Dict)
import Dir
import Level exposing (Level)
import Path
import RelativePos exposing (RelativePos)
import Set exposing (Set)
import Stage exposing (SavedStage, Stage)
import StaticArray.Index as Index


type alias Game =
    { stage : Stage
    , levels : Dict String (Dict Int SavedStage)
    , isConnected : Dict RelativePos { targetIds : Set Int }
    }


fromStage : Stage -> Game
fromStage stage =
    { stage = stage
    , levels = Dict.empty
    , isConnected = Dict.empty
    }


loadStage : Stage -> Game -> Game
loadStage stage game =
    { game | stage = stage, isConnected = Dict.empty }


isSolved : Level -> Game -> Bool
isSolved level game =
    game.stage.targets
        |> Dict.toList
        |> List.all
            (\( pos, _ ) ->
                case game.stage.grid |> Dict.get pos of
                    Just (Target target) ->
                        (Dict.size target.from == Config.powerStrengths level)
                            && (target.from
                                    |> Dict.values
                                    |> List.map .originId
                                    |> Set.fromList
                                    |> Set.size
                                    |> (==) 1
                               )

                    _ ->
                        False
            )


saveLevel : { level : Level, stage : Int } -> Game -> Game
saveLevel args game =
    let
        maxPos =
            game.levels
                |> Dict.get (args.level |> Level.previous |> Maybe.withDefault Index.first |> Level.toString)
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.head
                |> Maybe.map (\( _, { gridSize } ) -> gridSize)
                |> Maybe.withDefault 1
    in
    { game
        | levels =
            game
                |> toSave { level = args.level, maxPos = maxPos }
                |> Maybe.map
                    (\savedGame ->
                        game.levels
                            |> Dict.update (Level.toString args.level)
                                (\maybe ->
                                    maybe
                                        |> Maybe.withDefault Dict.empty
                                        |> Dict.insert args.stage savedGame
                                        |> Just
                                )
                    )
                |> Maybe.withDefault game.levels
    }


toSave : { level : Level, maxPos : Int } -> Game -> Maybe SavedStage
toSave args game =
    let
        list =
            game.stage
                |> Path.build args

        connections :
            Dict
                RelativePos
                { from : RelativePos
                , originId : Int
                , path : List RelativePos
                }
        connections =
            list
                |> List.map
                    (\{ from, to, path, originId } ->
                        [ ( from
                          , { from = to
                            , originId = originId
                            , path = path
                            }
                          )
                        , ( to
                          , { from = from
                            , originId = originId
                            , path = path
                            }
                          )
                        ]
                    )
                |> List.concat
                |> Dict.fromList

        paths : Dict RelativePos { origins : Set Int }
        paths =
            list
                |> List.foldl
                    (\{ path, originId } d ->
                        path
                            |> List.foldl
                                (\pos ->
                                    Dict.update pos
                                        (\maybe ->
                                            { origins =
                                                maybe
                                                    |> Maybe.map (\{ origins } -> origins |> Set.insert originId)
                                                    |> Maybe.withDefault (Set.singleton originId)
                                            }
                                                |> Just
                                        )
                                )
                                d
                    )
                    Dict.empty

        grid : Dict RelativePos Cell
        grid =
            game.stage.grid
                |> Dict.toList
                |> List.map (\( k, v ) -> ( RelativePos.fromTuple k, v ))
                |> Dict.fromList
    in
    { connections = connections
    , paths = paths
    , grid = grid
    , level = args.level
    , gridSize = game.stage.gridSize
    }
        |> Just


tick :
    { computeActiveConnections : ( ( Int, Int ), Connection ) -> Stage -> Connection
    , level : Level
    , maxPos : Int
    }
    -> Game
    -> ( Game, Bool )
tick args game =
    let
        maxPos =
            { maxPos = args.maxPos }
    in
    game.stage.grid
        |> Dict.map
            (\pos cell ->
                case cell of
                    ConnectionCell conncetion ->
                        game.stage
                            |> args.computeActiveConnections ( pos, conncetion )
                            |> ConnectionCell

                    Target { id } ->
                        RelativePos.list maxPos
                            |> List.map
                                (\relPos ->
                                    { pos =
                                        relPos
                                            |> RelativePos.toDir maxPos
                                            |> Dir.addTo pos
                                    , from = relPos
                                    , to =
                                        relPos
                                            |> RelativePos.reverse maxPos
                                    }
                                )
                            |> List.filterMap
                                (\dir ->
                                    if
                                        game.stage.origins
                                            |> Dict.member dir.pos
                                    then
                                        Nothing

                                    else
                                        dir.pos
                                            |> Stage.sendsEnergy
                                                { to = dir.to
                                                }
                                                game.stage
                                            |> Maybe.map
                                                (\{ originId } ->
                                                    ( dir.from, { originId = originId } )
                                                )
                                )
                            |> Dict.fromList
                            |> (\from -> Target { from = from, id = id })

                    _ ->
                        cell
            )
        |> (\d ->
                ( { game | stage = game.stage |> (\stage -> { stage | grid = d }) }
                    |> (\g ->
                            { g
                                | isConnected =
                                    g.stage
                                        |> Path.build
                                            { level = args.level
                                            , maxPos = args.maxPos
                                            }
                                        |> Path.toDict
                            }
                       )
                , Dict.toList d /= Dict.toList game.stage.grid
                )
           )


update : Level -> Game -> ( Game, Bool )
update level game =
    let
        stages =
            level
                |> Level.previous
                |> Maybe.andThen
                    (\previousLevel ->
                        game.levels
                            |> Dict.get (Level.toString previousLevel)
                    )
                |> Maybe.withDefault Dict.empty

        maxPos =
            stages
                |> Dict.toList
                |> List.head
                |> Maybe.map (\( _, { gridSize } ) -> gridSize)
                |> Maybe.withDefault 1

        neighborsDirLevel1 pos stage =
            Dir.list
                |> List.map (Dir.addTo ( 0, 0 ))
                |> List.map RelativePos.fromTuple
                |> List.filterMap
                    (\dir ->
                        case
                            Dict.get
                                (dir
                                    |> RelativePos.toDir { maxPos = maxPos }
                                    |> Dir.addTo pos
                                )
                                stage.grid
                        of
                            Just (ConnectionCell _) ->
                                Just dir

                            Just (Origin _) ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )
    in
    if level == Index.first then
        tick
            { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv1 (neighborsDirLevel1 pos game.stage) ( pos, a )
            , level = level
            , maxPos = maxPos
            }
            game

    else
        tick
            { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsGeneric stages a pos
            , level = level
            , maxPos = maxPos
            }
            game


clearStage : Level -> Game -> Game
clearStage level game =
    { game | stage = game.stage |> Stage.clear level }
