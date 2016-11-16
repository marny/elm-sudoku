module ValidateBoard exposing (valid)

import Board exposing (..)
import Set exposing (..)

valid : Board -> (Bool, List String)
valid board = 
    let 
        (okCompleted, errorNotCompleted) = allBoxesCompleted board 
        rowErrorMessages = validateRows board
        columnErrorMessages = validateColumns board
        totalErrorMessages = if not okCompleted then errorNotCompleted :: (rowErrorMessages ++ columnErrorMessages) else rowErrorMessages ++ columnErrorMessages
    in 
        (List.isEmpty totalErrorMessages, totalErrorMessages)

allBoxesCompleted : Board -> (Bool, String)
allBoxesCompleted board =
    let 
        totalCount = noOfRows * noOfColumns
        missingCount = totalCount - List.length board.boxes
        ok = missingCount == 0
     in
      (ok, "There are " ++ toString missingCount ++ " boxes left.")   
   

validateRows : Board -> List String
validateRows board =
    validateBoxes (\row -> validRow row board)
    

validateBoxes : (Int -> (Bool, String)) -> List String
validateBoxes fn =
    let
        boxes = List.map fn [1..9]
        boxesWithErrors = List.filter (\(ok, msg) -> not ok) boxes
    in
        List.map (\(ok, msg) -> msg) boxesWithErrors
        

validRow : Int -> Board -> (Bool, String)
validRow row board =
    let
        numbers = uniqueNumbers (\box -> box.row == row) board
    in
        (Set.size numbers == noOfRows, "Row " ++ toString row ++ " has duplicates or missing values" )

validateColumns : Board -> List String
validateColumns board =
     validateBoxes (\col -> validColumn col board)
    

validColumn : Int -> Board -> (Bool, String)
validColumn column board =
    let
        numbers = uniqueNumbers (\box -> box.column == column) board
    in
        (Set.size numbers == noOfColumns, "Column " ++ toString column ++ " has duplicates or missing values" )

uniqueNumbers : (Box -> Bool) -> Board -> Set Int
uniqueNumbers fn board =
    let
        boxes = List.filter fn board.boxes
        numberInBoxes = List.map (\box -> box.value) boxes       
    in
        Set.fromList numberInBoxes  
