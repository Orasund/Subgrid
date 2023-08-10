module View.Render exposing (..)

import Cell exposing (Cell(..))
import Color
import Config exposing (powerStrengths)
import Dict
import Level exposing (Level)
import Svg exposing (Svg)
import Svg.Attributes
import View.Svg exposing (RenderFunction)


cellRender : Level -> Cell -> RenderFunction msg
cellRender level cell =
    case cell of
        ConnectionCell _ ->
            boxRender

        Wall ->
            boxRender

        Origin _ ->
            boxRender

        Target { from } ->
            case from |> Dict.toList of
                [ ( _, { originId } ) ] ->
                    if Dict.size from == Config.powerStrengths level then
                        targetRender
                            { secondaryColor = Color.wallColor
                            , trinaryColor = Color.wallColor
                            , variant = Config.powerStrengths level
                            , small = False
                            , fill = True
                            }

                    else
                        targetRender
                            { secondaryColor = Color.laserColor level originId
                            , trinaryColor = Color.wallColor
                            , variant = Config.powerStrengths level
                            , small = False
                            , fill = False
                            }

                [ ( _, from1 ), ( _, from2 ) ] ->
                    if from1.originId == from2.originId then
                        targetRender
                            { secondaryColor = Color.wallColor
                            , trinaryColor = Color.wallColor
                            , variant = Config.powerStrengths level
                            , small = False
                            , fill = True
                            }

                    else
                        targetRender
                            { secondaryColor = Color.laserColor level from1.originId
                            , trinaryColor = Color.laserColor level from2.originId
                            , variant = Config.powerStrengths level
                            , small = False
                            , fill = False
                            }

                _ ->
                    targetRender
                        { secondaryColor = Color.wallColor
                        , trinaryColor = Color.wallColor
                        , variant = Config.powerStrengths level
                        , small = False
                        , fill = False
                        }


targetRender : { secondaryColor : String, trinaryColor : String, variant : Int, small : Bool, fill : Bool } -> RenderFunction msg
targetRender { secondaryColor, trinaryColor, variant, small, fill } args =
    let
        ( x, y ) =
            args.pos

        size =
            (if small then
                toFloat args.size / 8

             else
                toFloat args.size / 4
            )
                |> (\f ->
                        if fill then
                            f * 1.25

                        else
                            f * 1
                   )

        baseattrs color =
            if fill then
                [ Svg.Attributes.fill color ]

            else
                [ Svg.Attributes.strokeWidth
                    (size / 3 |> String.fromFloat)
                , Svg.Attributes.stroke color
                , Svg.Attributes.fill "none"
                ]
    in
    Svg.rect
        [ Svg.Attributes.width (args.size |> String.fromInt)
        , Svg.Attributes.height (args.size |> String.fromInt)
        , Svg.Attributes.fill args.color
        , Svg.Attributes.mask "url(#no-power)"
        , Svg.Attributes.x (x |> String.fromInt)
        , Svg.Attributes.y (y |> String.fromInt)
        ]
        []
        :: (case variant of
                2 ->
                    [ Svg.circle
                        ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.r (size |> String.fromFloat)
                         ]
                            ++ baseattrs secondaryColor
                        )
                        []
                    , Svg.circle
                        ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.r (size / 2 |> String.fromFloat)
                         ]
                            ++ baseattrs trinaryColor
                        )
                        []
                    ]

                _ ->
                    [ Svg.circle
                        ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.r (size |> String.fromFloat)
                         ]
                            ++ baseattrs secondaryColor
                        )
                        []
                    ]
           )
        |> Svg.g []


boxRender : { pos : ( Int, Int ), color : String, size : Int } -> Svg msg
boxRender args =
    let
        ( x, y ) =
            args.pos
    in
    Svg.rect
        [ Svg.Attributes.x (x |> String.fromInt)
        , Svg.Attributes.y (y |> String.fromInt)
        , Svg.Attributes.width (args.size |> String.fromInt)
        , Svg.Attributes.height (args.size |> String.fromInt)
        , Svg.Attributes.fill args.color
        ]
        []


connectionRender : { pos : ( Int, Int ), color : String, size : Int } -> Svg msg
connectionRender args =
    let
        ( x, y ) =
            args.pos
    in
    Svg.circle
        [ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.r (toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.fill args.color
        ]
        []
