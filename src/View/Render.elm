module View.Render exposing (..)

import Cell exposing (Cell(..))
import Color
import Config exposing (powerStrengths)
import Dict
import Svg exposing (Svg)
import Svg.Attributes
import View.Svg exposing (RenderFunction)


cellRender : { powerStrengths : Int } -> Cell -> RenderFunction msg
cellRender args cell =
    case cell of
        ConnectionCell _ ->
            boxRender

        Wall ->
            boxRender

        Origin _ ->
            boxRender

        Target { id, from } ->
            case from |> Dict.toList of
                [ _ ] ->
                    targetRender
                        { secondaryColor = Color.wallColor
                        , variant = args.powerStrengths
                        , small = False
                        , fill = True
                        }

                _ ->
                    targetRender
                        { secondaryColor = Color.wallColor
                        , variant = args.powerStrengths
                        , small = False
                        , fill = False
                        }


targetRender : { secondaryColor : String, variant : Int, small : Bool, fill : Bool } -> RenderFunction msg
targetRender { secondaryColor, variant, small, fill } args =
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

        baseattrs =
            if fill then
                [ Svg.Attributes.fill secondaryColor ]

            else
                [ Svg.Attributes.strokeWidth
                    (size / 3 |> String.fromFloat)
                , Svg.Attributes.stroke secondaryColor
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
                            ++ baseattrs
                        )
                        []
                    , Svg.circle
                        ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.r (size / 2 |> String.fromFloat)
                         ]
                            ++ baseattrs
                        )
                        []
                    ]

                _ ->
                    [ Svg.circle
                        ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                         , Svg.Attributes.r (size |> String.fromFloat)
                         ]
                            ++ baseattrs
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
