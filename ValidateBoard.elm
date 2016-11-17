module ValidateBoard exposing (valid, allBoxesCompleted)

import Board exposing (..)
import Set exposing (..)

valid : Board -> (Bool, List String)
valid board = 
    let 
        rowErrorMessages = validateRows board
        columnErrorMessages = validateColumns board
        quadrantErrorMessages = validateQuadrants board
        totalErrorMessages = rowErrorMessages ++ columnErrorMessages ++ quadrantErrorMessages
    in 
        (List.isEmpty totalErrorMessages, totalErrorMessages)

allBoxesCompleted : Board -> Bool
allBoxesCompleted board =
    (List.length board.boxes /= 81)


validateRows : Board -> List String
validateRows board =
    validateBoxes (\row -> validRow row board)
    

validateBoxes : (Int -> (Bool, String)) -> List String
validateBoxes fn =
    let
        boxes = List.map fn (List.range 1 9)
        boxesWithErrors = List.filter (\(ok, msg) -> not ok) boxes
    in
        List.map (\(ok, msg) -> msg) boxesWithErrors
        

validRow : Int -> Board -> (Bool, String)
validRow row board =
    let
        numbers = uniqueNumbers (\box -> box.row == row) board
    in
        (Set.size numbers == noOfRows, "Row " ++ toString row ++ " has duplicates or missing values." )

validateColumns : Board -> List String
validateColumns board =
     validateBoxes (\col -> validColumn col board)
    

validColumn : Int -> Board -> (Bool, String)
validColumn column board =
    let
        numbers = uniqueNumbers (\box -> box.column == column) board
    in
        (Set.size numbers == noOfColumns, "Column " ++ toString column ++ " has duplicates or missing values." )


uniqueNumbers : (Box -> Bool) -> Board -> Set Int
uniqueNumbers fn board =
    let
        boxes = List.filter fn board.boxes
        numberInBoxes = List.map .value boxes       
    in
        Set.fromList numberInBoxes  


validateQuadrants : Board -> List String
validateQuadrants board =
    validateBoxes (\id -> validateQuadrant id board)


validateQuadrant : Int -> Board -> (Bool, String) 
validateQuadrant quadrant board =
    let 
        numbers = uniqueNumbers (\box -> Board.getQuadrant box == quadrant) board
    in
        (Set.size numbers == 9, "Quadrant " ++ toString quadrant ++ " has duplicates or missing values.")





