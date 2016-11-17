module Sudoku exposing (..)

import Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (maxlength, class, classList, type_, id, disabled, tabindex, style)
import Html.Events exposing (onClick, onInput, onDoubleClick, onFocus)
import Html.Keyed as Keyed
import Key exposing (..)
import Keyboard
import String
import ValidateBoard exposing (..)
import Debug
import Http exposing (get)
import Task exposing (Task)
import Json.Decode as Json exposing (field)

-- MODEL

type alias Model = 
    { id : String
    , board : Board
    , validationErrors : List String
    , selectedBox : Box
    }


init : Model
init = 
    { id = "DN 2016-11-04 Easy"
    , board = Board.create
    , validationErrors = []
    , selectedBox = Board.emptyBox
    }


-- UPDATE
type Msg = Update Box 
        | Reset 
        | Validate 
        | Highlight Box 
        | Remove Box 
        | KeyDown Int 
        | New 
        | DataFetched (Result Http.Error (List Box))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        board = model.board
        keepOnBoard filterFunction = List.filter filterFunction model.board.boxes
    in
    case msg of 
        Update boxUpdated ->  
            let 
                boardWithoutBoxToUpdate = keepOnBoard (Board.boxNotEqual boxUpdated)
                newBoard = { board | boxes = (boxUpdated :: boardWithoutBoxToUpdate) }
            in 
               { model 
                    | board = newBoard
                    , selectedBox = boxUpdated} ![]

        Reset -> 
            let 
                onlyLockedBoxes = List.filter (\box -> box.locked) model.board.boxes
                newBoard = { board | boxes = onlyLockedBoxes}
            in
                { model 
                    | board = newBoard } ![]


        Validate -> 
            let 
                (ok, errorList) = ValidateBoard.valid model.board           
            in
            { model | validationErrors = errorList } ![]

        Highlight box ->
            { model | selectedBox = box } ![]

        Remove boxToRemove ->
            let
                boardWithoutRemovedBox = keepOnBoard (Board.boxNotEqual boxToRemove)
                newBoard = { board | boxes = boardWithoutRemovedBox }
            in
                { model 
                    | board = newBoard
                    , selectedBox = boxToRemove} ![]
        
        KeyDown key ->
            let 
                modelUpdated = { model | selectedBox = keyDownValue key model.selectedBox}
                boxUpdated = modelUpdated.selectedBox
                boardWithoutBoxToUpdate = keepOnBoard (Board.boxNotEqual boxUpdated)
                newBoard = { board | boxes = (boxUpdated :: boardWithoutBoxToUpdate) }
            in 
               { modelUpdated 
                    | board = newBoard } ![]

        New -> 
            ({model | selectedBox = Board.emptyBox }, fetchNewSudoku)

        DataFetched (Ok data) ->
            let
                filteredBoxes = List.filter (\box -> box.value /= 0) data
                newBoard = { board | boxes = filteredBoxes}
            in
                { model | board = newBoard} ! []

        DataFetched (Err error)  ->
            let
                errorMessage = 
                    case error of
                        Http.BadUrl url -> "Bad url: " ++ url
                        Http.BadStatus resp -> "Bad status:" ++ resp.body
                        Http.BadPayload str resp -> str
                        Http.Timeout -> "Timeout"
                        Http.NetworkError -> "Network error. Check console for details."
                       
            in
            { model | validationErrors = ["Oops! An error occurred: " ++ errorMessage] } ! []


keyDownValue : Int -> Box -> Box
keyDownValue key box =
    let
        valueToUpdate = if box.locked then box.value else Key.fromCode key
    in
    { box | value = valueToUpdate }
    


fetchNewSudoku : Cmd Msg
fetchNewSudoku =
    let 
        request = Http.get "http://localhost:3000/sudoku/new" sudokuDecoder 
    in
        Http.send DataFetched request



sudokuDecoder : Json.Decoder (List Box)
sudokuDecoder =
  Json.list boxDecoder


boxDecoder : Json.Decoder Box
boxDecoder =
    Json.map4 Box
        (Json.field "value" Json.int)
        (Json.field "locked" Json.bool)
        (Json.field "column" Json.int)
        (Json.field "row" Json.int)


-- VIEW
view : Model -> Html Msg
view model = 
    let
        anyBoxSelected = model.selectedBox.value > 0
        boxCanBeUpdated = not model.selectedBox.locked 
        createBox number = 
            let
                value = 
                    if boxCanBeUpdated then number else model.selectedBox.value
            in
                Box value model.selectedBox.locked model.selectedBox.column model.selectedBox.row
    in
        div 
            [] 
            [h3 
                [class "cover-heading"] 
                [text model.id]
            ,table 
                [class "table table-bordered fat-border pull-left"] 
                [tbody [] (tableBody model)]
            ,div 
                [style [("width","50px")]
                ,class "pull-left"] 
                [p [] []]
            ,table
                [class "table table-bordered fat-border table-slim pull-left"]
                [thead [] [summaryHeader]
                ,tbody [] (summaryTable model)]
            ,div 
                [class "btn-group pull-right"] 
                [button 
                    [type_ "button", class "btn btn-default", onClick Reset] 
                    [text "Reset"]
                ,button 
                    [type_ "button"
                    , class "btn btn-default"
                    , onClick Validate 
                    , disabled (ValidateBoard.allBoxesCompleted model.board)
                    ]
                    [text "Validate"]
                ,button 
                    [type_ "button", class "btn btn-default", onClick New] 
                    [text "New"]
                ]
            ,div [class "clearfix"] []
            ,div 
                [] 
                [Keyed.ul 
                    [class "list-group"] <| List.map viewKeyedEntry model.validationErrors]               
            ]
    

viewKeyedEntry : String -> ( String, Html Msg )
viewKeyedEntry error =
    ( error, li [class "list-group-item list-group-item-danger"] [text error] )


tableBody : Model -> List (Html Msg)
tableBody model =  
    List.map (rows model) (List.range 1 noOfRows)

    
rows : Model -> Int -> Html Msg 
rows model row = 
    let
        modOfThree = row % 3 == 0
    in
        tr 
            [classList [("fat-border-bottom", modOfThree)]] 
            (List.map (oneRow model row) (List.range 1 noOfColumns))


oneRow : Model -> Int -> Int -> Html Msg 
oneRow model row column = 
    let 
        box = getBox column row model.board
        modOfThree = column % 3 == 0
        highlight = box.value == model.selectedBox.value
        boxSelected = not (boxNotEqual box model.selectedBox) 
        castToInt str = String.toInt str |> Result.toMaybe |> Maybe.withDefault 
        tabindexCount column row = column + (row - 1) * 9
    in
        if box.locked then  
            td 
                [classList 
                    [("fat-border-right", modOfThree)
                    ,("text-center", True)
                    ,("bg-primary", highlight)
                   
                    ]
                , tabindex (tabindexCount column row)
                , onFocus (Highlight box)
                ] 
                [ strong 
                    [] 
                    [text (toString box.value)]
                ]
        else if box.value > 0 then
            td 
                [ classList 
                    [("text-center", True) 
                    ,("fat-border-right", modOfThree)
                    ,("bg-primary", highlight)
                    ,("grey", not highlight)                    
                    ] 
                , tabindex (tabindexCount column row)
                , onFocus (Highlight box)
                , onDoubleClick (Remove (Box 0 False column row))
                ] 
                [ text (toString box.value)]
        else
            td 
                [ classList 
                    [("grey", not boxSelected)
                    ,("fat-border-right", modOfThree)
                    ,("grey-selected", boxSelected)
                   
                    ]
                , tabindex (tabindexCount column row)
                , onFocus (Highlight (Box 0 False column row))] 
                []


summaryTable : Model -> List (Html Msg)
summaryTable model =  
    List.map (summaryRow model) (List.range 1 noOfRows)


summaryRow : Model -> Int -> Html Msg 
summaryRow model num =
    let 
        count = Board.count num model.board
        perfect = count == 9
        toHigh = count > 9
    in
        tr 
            [] 
            [td 
                [class "text-center grey"] 
                [text (toString num)]
            ,td 
                [classList 
                    [("bg-success", perfect)
                    ,("bg-danger", toHigh)
                    ,("text-center", True)
                    ]
                ]
                [text (toString count)]
            ]

summaryHeader : Html Msg
summaryHeader = 
    tr 
        [] 
        [td [class "text-center"]
            [strong [] [text "no"]]
        ,td [class "text-center"]
            [strong [] [text "#"]]
        ]
    


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        ]


-- MAIN
main : Program Never Model Msg
main = program 
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
