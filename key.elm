module Key exposing (..)


fromCode : Int -> Int
fromCode keyCode =
    case keyCode of
        49 ->
            1

        50 ->
            2

        51 ->
            3

        52 -> 
            4

        53 ->
            5

        54 ->
            6

        55 ->
            7

        56 ->
            8

        57 ->
            9

        _ ->
            0