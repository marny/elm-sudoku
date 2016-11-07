module Sudoku exposing (..)

import Board exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (maxlength, class, classList, style, type', id)
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Html.Keyed as Keyed
import Key exposing (..)
import Keyboard
import String



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
    , selectedBox = Box 0 False 0 0
    }


-- UPDATE
type Msg = Update Box | Reset | Validate | Highlight Box | Remove Box | KeyDown Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
     --  boxNotEqual box1 box2 = box1.column /= box2.column || box1.row /= box2.row
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

        Reset -> (init, Cmd.none)

        Validate -> 
            let 
                (ok, errorList) = valid model.board           
                errorMsgs = 
                    if ok then
                        []
                    else 
                        errorList
            in
            { model | validationErrors = errorMsgs } ![]

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
                --debug2 = Debug.log "KeyDown 2" boxUpdated
                boardWithoutBoxToUpdate = keepOnBoard (Board.boxNotEqual boxUpdated)
                newBoard = { board | boxes = (boxUpdated :: boardWithoutBoxToUpdate) }
            in 
               { modelUpdated 
                    | board = newBoard } ![]


keyDownValue : Int -> Box -> Box
keyDownValue key box =
    let
        boxCanBeUpdated = not box.locked
        valueToUpdate = if boxCanBeUpdated then Key.fromCode key else box.value
    in
    { box | value = valueToUpdate }
    


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
        [h2 
            [class "cover-heading"] 
            [text model.id]
        ,table 
            [class "table table-bordered", style [("border", "2px solid #FFF")]] 
            [tbody [] (tableBody model)]
        ,div 
            [class "btn-group"] 
            [button 
                [type' "button", class "btn btn-default", onClick Reset] 
                [text "Reset"]
            ,button 
                [type' "button", class "btn btn-default", onClick Validate] 
                [text "Validate"]
            ]
        ,div 
            [class "text-danger"] 
            [Keyed.ul 
                [] <| List.map viewKeyedEntry model.validationErrors]   
        ]
    

viewKeyedEntry : String -> ( String, Html Msg )
viewKeyedEntry error =
    ( error, li [] [text error] )




tableBody : Model -> List (Html Msg)
tableBody model =  
    List.map (rows model) [1..noOfRows]

    
rows : Model -> Int -> Html Msg 
rows model row = 
    let
        borderStyle = 
            if row % 3 == 0 then
                [("border-bottom", "2px solid #FFF")]
            else
                []

    in
        tr 
            [style borderStyle] 
            (List.map (oneRow model row) [1..noOfColumns])


oneRow : Model -> Int -> Int -> Html Msg 
oneRow model row column = 
    let 
        box = getBox column row model.board

        borderStyle = 
            if column % 3 == 0 then
                [("border-right", "2px solid #FFF")]
            else
                []
        highlight = box.value == model.selectedBox.value
        boxSelected = not (boxNotEqual box model.selectedBox) 
        boxStyle = 
            if highlight then 
                borderStyle
            else
                ("background-color", "rgba(255,255,255,.25)") :: borderStyle 

        castToInt str = String.toInt str |> Result.toMaybe |> Maybe.withDefault 

    in
        if box.locked then  
            td 
                [ style borderStyle
                , classList 
                    [("text-center", True)
                    ,("bg-primary", highlight)
                    ]
                , onClick (Highlight box)
                ] 
                [ strong 
                    [] 
                    [text (toString box.value)]
                ]
        else if box.value > 0 then
            td 
                [ style borderStyle
                , classList 
                    [("text-center", True) 
                    ,("bg-primary", highlight)
                    ,("grey", not highlight)
                    ] 
                , onClick (Highlight box)
                , onDoubleClick (Remove (Box 0 False column row))
                ] 
                [ text (toString box.value)]
        else
            td 
                [ style borderStyle
                , classList 
                    [ ("grey", not boxSelected)
                    , ("grey-selected", boxSelected) ]
                , onClick (Highlight (Box 0 False column row))] 
                []


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        ]


-- MAIN
main : Program Never
main = App.program 
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
