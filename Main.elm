module Main exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Array exposing (Array)
import Time
import List
import String
import Random
import Lifegame


main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Lifegame.Lifegame


type Msg
    = New
    | SetY String
    | SetX String
    | Start
    | Next
    | Stop
    | Random
    | RandomSet (Array (Array Bool))


init : ( Model, Cmd Msg )
init =
    ( Lifegame.init 0 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isContiune then
        Time.every (Time.millisecond * 100) <| always Next
    else
        Sub.map (always Stop) Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            ( Lifegame.init model.size.y model.size.x, Cmd.none )

        SetY s ->
            case String.toInt s of
                Ok n ->
                    ( { model | size = { x = model.size.x, y = n } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SetX s ->
            case String.toInt s of
                Ok n ->
                    ( { model | size = { y = model.size.y, x = n } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Start ->
            ( { model | isContiune = True }, Cmd.none )

        Next ->
            ( Lifegame.next model, Cmd.none )

        Stop ->
            ( { model | isContiune = False }, Cmd.none )

        RandomSet cells ->
            ( { model | cells = cells }, Cmd.none )

        Random ->
            let
                gen =
                    Random.map Array.fromList <|
                        Random.list model.size.y <|
                            Random.map Array.fromList <|
                                Random.list model.size.x Random.bool
            in
                ( model, Random.generate RandomSet gen )


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div []
            [ H.input [ HA.type_ "number", HA.placeholder "y", HE.onInput SetY ] []
            , H.input [ HA.type_ "number", HA.placeholder "x", HE.onInput SetX ] []
            , H.button [ HE.onClick New ] [ H.text "New" ]
            , H.button [ HE.onClick Start ] [ H.text "Start" ]
            , H.button [ HE.onClick Stop ] [ H.text "Stop" ]
            , H.button [ HE.onClick Random ] [ H.text "Random" ]
            ]
        , H.div [] [ H.text (toString model.count) ]
        , H.div [] [ viewLifegame model ]
        ]


viewLifegame : Model -> H.Html Msg
viewLifegame model =
    S.svg
        [ SA.width (toString <| model.size.x * 50)
        , SA.height (toString <| model.size.y * 50)
        ]
        (List.concat <|
            Array.toList <|
                Array.indexedMap
                    (\y xs ->
                        Array.toList <|
                            Array.indexedMap
                                (\x b ->
                                    S.rect
                                        [ SA.x (toString <| x * 5)
                                        , SA.y (toString <| y * 5)
                                        , SA.width "5"
                                        , SA.height "5"
                                        , SA.fill
                                            (if b then
                                                "green"
                                             else
                                                "black"
                                            )
                                        , SA.stroke "black"
                                        ]
                                        []
                                )
                                xs
                    )
                    model.cells
        )
