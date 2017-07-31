module Lifegame exposing (..)

import Array exposing (Array)


type alias Lifegame =
    { size : { y : Int, x : Int }
    , count : Int
    , isContiune : Bool
    , cells : Array (Array Bool)
    }


init : Int -> Int -> Lifegame
init y x =
    { size = { y = y, x = x }
    , count = 0
    , isContiune = False
    , cells = Array.repeat y <| Array.repeat x False
    }


rule : Bool -> Int -> Bool
rule b n =
    case ( b, n ) of
        ( True, 2 ) ->
            True

        ( True, 3 ) ->
            True

        ( False, 3 ) ->
            True

        _ ->
            False


env : Int -> Int -> Lifegame -> Int
env y x lg =
    let
        arounds =
            [ ( y - 1, x - 1 )
            , ( y - 1, x )
            , ( y - 1, x + 1 )
            , ( y, x - 1 )
            , ( y, x + 1 )
            , ( y + 1, x - 1 )
            , ( y + 1, x )
            , ( y + 1, x + 1 )
            ]

        f ( y, x ) =
            case Array.get y lg.cells of
                Nothing ->
                    0

                Just xs ->
                    case Array.get x xs of
                        Nothing ->
                            0

                        Just b ->
                            if b then
                                1
                            else
                                0
    in
        List.sum <| List.map f arounds


next : Lifegame -> Lifegame
next lg =
    let
        cells =
            Array.indexedMap
                (\y xs ->
                    Array.indexedMap
                        (\x b -> rule b <| env y x lg)
                        xs
                )
                lg.cells
    in
        { lg
            | count = lg.count + 1
            , cells = cells
        }
