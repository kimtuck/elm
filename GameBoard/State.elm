module GameBoard.State exposing (init,update,subscriptions)

import Game.Helpers exposing (..)
import GameBoard.Types exposing (..)

init : ( Model, Cmd Msg )
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  {
     board = newBoard 3 4
     , rows = 3
     , columns = 4
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    CreateBoard ->
        ( { model | board = newBoard 3 4 }, Cmd.none)

    UpdateRows rows ->
        let newRows = toIntOrDefault model.rows rows
        in
        ( { model | rows = setIfInRange newRows model.rows 2 model.maxRows, board = newBoard newRows model.columns, currentPlayer = Player1  } , Cmd.none )

    UpdateColumns columns ->
        let newCols = toIntOrDefault model.columns columns
         in
        ( {model | columns = setIfInRange newCols model.columns 2 model.maxColumns, board = newBoard model.rows newCols, currentPlayer = Player1 } , Cmd.none)

    ClickSide (side) ->
        ( updateTurn model side, Cmd.none)

    NewGame ->
        ( { model | score = (Score 0 0), gameOver = False, currentPlayer = Player1, board = newBoard model.rows model.columns, showForm = True} , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

newBoard: Int ->  Int -> Board
newBoard rows columns =

    let squares = sqGrid rows columns
        sides =   sideGrid ((2 * rows) + 1) (columns + 1)
    in
        Board squares  sides

sqlist: Int -> Int -> List Square
sqlist row columns =
        List.map (\column -> Square (Location row column) None) (seq columns)

sqGrid: Int -> Int -> List Square
sqGrid rows columns =
        List.concatMap (\row -> sqlist row columns) (seq rows)

sidelist: Int -> Int -> List Side
sidelist row columns =
        List.map (\column -> Side (Location row column) None) (seq (columns + 1))

sideGrid: Int -> Int -> List Side
sideGrid rows columns =
        List.concatMap (\row -> sidelist row columns) (seq (rows + 1))

updateTurn: Model -> Side -> Model
updateTurn model side =
    updateSquares model
    --closeForm model
    --|> resetPlayerCompletedSquare
    --|> updateSide side
    --|> updateSquares
    --|> updatePlayer
    --|> checkGameOver

updateSide: Side -> Model -> Model
updateSide side model =
    let
        z=Debug.log "clicked side" side
        sides = List.map (\e ->
            if side == e then
                { e | owner = Player1 }
            else
                e
                ) model.board.sides
        b = Board model.board.squares sides
    in
        { model | board = b }

updateSquares: Model -> Model
updateSquares model =
    let
        m1 = model
        currentPlayerSquares = counter m1.board.squares (\square -> square.owner == m1.currentPlayer)
        m2 = { m1 | board = { squares =
                (updateSquareOwnership m1.board.squares m1.board.sides m1.currentPlayer)
            , sides = model.board.sides } }
        newPlayerSquares = counter m2.board.squares (\square -> square.owner == m2.currentPlayer)
        m3 = { m2 | currentPlayerCompletedSquare = currentPlayerSquares /= newPlayerSquares }
        m4 = { m3 | score = { player1 = (counter m3.board.squares (\square -> square.owner == Player1)), player2 = (counter m3.board.squares (\square -> square.owner == Player2)) } }
    in
        m4

updateSquareOwnership: List Square -> List Side -> Owner -> List Square
updateSquareOwnership squares sides owner =
    List.map (\ square ->
        if square.owner == None && (countAdjacentSides square sides) == 4 then
            { square | owner = Debug.log ("set owner " ++ (toString square) ++ " owner " ++ (toString owner) ) owner }
        else
            square
        ) squares

countAdjacentSides: Square -> List Side -> Int
countAdjacentSides square sides =
    List.foldl (\side accum ->
        if side.owner /= None
           && isAdjacentSide square side
        then
           Debug.log "Accum" (accum + 1)
        else
            accum
    ) 0 sides

isAdjacentSide: Square -> Side -> Bool
isAdjacentSide square side =
    let isAdjacent =
         -- Top
         side.location.row == 2 * square.location.row && side.location.column == square.location.column
         -- bottom
         || side.location.row == (2 * square.location.row + 2) && side.location.column == square.location.column
         -- left
         || side.location.row == (2 * square.location.row + 1) && side.location.column == square.location.column
         -- right
         || side.location.row == (2 * square.location.row + 1) && side.location.column == square.location.column + 1
       in
         if isAdjacent then
            Debug.log ("isAdjacentSide "++ (toString square) ++ " " ++ (toString side)) isAdjacent
         else
            isAdjacent
