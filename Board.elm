module Board exposing (..)

import String

(noOfColumns, noOfRows) = (9, 9)

type alias Board =
    { boxes : List Box
    }


type alias Box =
    { value : Int
    , locked : Bool
    , column : Int
    , row : Int
    }


create : Board
create = 
    Board [Box 6 True 4 1, Box 1 True 5 1, Box 9 True 6 1, Box 2 True 7 1, Box 7 True 8 1, Box 5 True 9 1
          ,Box 6 True 2 2
          ,Box 1 True 1 3, Box 9 True 2 3, Box 7 True 3 3, Box 4 True 5 3, Box 8 True 8 3
          ,Box 7 True 1 4, Box 5 True 2 4, Box 6 True 3 4, Box 9 True 4 4, Box 2 True 6 4
          ,Box 8 True 1 5, Box 1 True 2 5, Box 5 True 7 5
          ,Box 9 True 3 6, Box 1 True 4 6, Box 7 True 7 6, Box 6 True 8 6
          ,Box 4 True 2 7, Box 5 True 4 7, Box 1 True 8 7
          ,Box 9 True 1 8, Box 8 True 2 8, Box 4 True 4 8, Box 5 True 8 8, Box 7 True 9 8 
          ,Box 1 True 3 9, Box 9 True 5 9, Box 8 True 6 9
          ]



getBox : Int -> Int -> Board -> Box
getBox column row board =
    let
        matchingBoxes = List.filter (\box -> box.column == column && box.row == row)  board.boxes
        matchingBox = List.head matchingBoxes
    in
        Maybe.withDefault (Box 0 False column row) (List.head matchingBoxes)


boxNotEqual  : Box -> Box -> Bool
boxNotEqual box1 box2 =
     box1.column /= box2.column || box1.row /= box2.row


valid : Board -> (Bool, List String)
valid board = 
    let 
        errorNotCompleted = 
            if List.length board.boxes /= (noOfRows * noOfColumns) then
                "Sudoku has empty boxes."
            else 
                ""

        totalErrorMessages = errorNotCompleted :: []
        errorMessages = totalErrorMessages
    in        
        (List.isEmpty errorMessages, errorMessages)
