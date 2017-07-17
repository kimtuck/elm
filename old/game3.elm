import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import GameBoard exposing (..)

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL


type alias Model =
  {
    namePlayer1: String
    , namePlayer2: String
    , board: Board
    , score: Score
    , gameOver: Bool
    , currentPlayer: Owner
    , currentPlayerCompletedSquare: Bool
    , rows: Int
    , columns: Int
    , maxRows: Int
    , maxColumns: Int
    , showForm: Bool
  }



-- INIT
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  {
    namePlayer1 = "Player1"
    , namePlayer2 = "Player2"
     , board = newBoard 3 4
     , score = Score 0 0
     , gameOver = False
     , currentPlayer = Player1
     , currentPlayerCompletedSquare = False
     , rows = 3
     , columns = 4
     , maxRows = 20
     , maxColumns = 20
     , showForm = True
  }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE
type Msg =
    CreateBoard
    | UpdateRows String
    | UpdateColumns String
    | ClickSide (Side)
    | NewGame
    | UpdateNamePlayer1 String
    | UpdateNamePlayer2 String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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

    UpdateNamePlayer1 name ->
        ( { model | namePlayer1 = name }, Cmd.none)

    UpdateNamePlayer2 name ->
        ( { model | namePlayer2 = name }, Cmd.none)

    ClickSide (side) ->
        ( updateTurn model side, Cmd.none)

    NewGame ->
        ( { model | score = (Score 0 0), gameOver = False, currentPlayer = Player1, board = newBoard model.rows model.columns, showForm = True} , Cmd.none )


updateTurn: Model -> Side -> Model
updateTurn model side =
    closeForm model
    |> resetPlayerCompletedSquare
    |> updateSide side
    |> updateSquares
    |> updatePlayer
    |> checkGameOver

closeForm model =
    { model | showForm = False }

resetPlayerCompletedSquare: Model -> Model
resetPlayerCompletedSquare model =
    { model | currentPlayerCompletedSquare = False }

updatePlayer model =
    let changePlayer = Debug.log "changePlayer? " (model.currentPlayerCompletedSquare == False)
        newPlayer = if changePlayer then
            case model.currentPlayer of
                Player1 ->
                    Player2
                Player2 ->
                    Player1
                _ ->
                    Player1
            else
                model.currentPlayer
    in
    { model | currentPlayer = newPlayer }

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


checkGameOver: Model -> Model
checkGameOver model =
    { model | gameOver = (counter model.board.squares (\square -> square.owner /= None)) == model.rows * model.columns }
-- VIEW


counter : List a -> (a -> Bool) -> Int
counter items fn =
    List.foldr (\item accum -> if (fn item) then accum + 1 else accum) 0 items

zipper : List a -> List a -> List a
zipper a b =
  case (a, b) of
    ( aa :: aBack, bb :: bBack ) ->
        [ aa, bb ] ++ zipper aBack bBack

    (_, _) ->
        []

type SideDirection = Horizontal | Vertical
type Element =
    ElementSquare Square

type alias Row =
    {
        row: List Square
    }

stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "example.css"
            ]
        children = []
    in
        node tag attrs children

-- Left side
findSide: Model -> Int -> Int -> Maybe Side
findSide model row column =
    List.filter (\e -> e.location.row == row && e.location.column == column) model.board.sides
    |> List.head

findSquare: Model -> Int -> Int -> Maybe Side
findSquare model row column =
    List.filter (\e -> e.location.row == row && e.location.column == column) model.board.squares
    |> List.head

-- Left side
--------------------------------------------------------------------------------------

classList: List String -> String
classList classes =
    String.concat (List.intersperse " " classes)

maybeCons : Maybe a -> List a -> List a
maybeCons maybeItem list =
     case maybeItem of
          Just item -> item :: list
          Nothing -> list


corner =
    div [class "corner" ] []

side: Model -> Side -> SideDirection -> Html Msg
side model elem direction =
    let
        cls = case direction of
            Horizontal ->
                "horizontal"
            Vertical ->
                "vertical"

        ownercls = case elem.owner of
            None -> "owner-none"
            Player1 -> "owner-player1"
            Player2 -> "owner-player2"

        click = case elem.owner of
            None -> Just <| onClick (ClickSide elem)
            Player1 -> Nothing
            Player2 -> Nothing
    in

        div (
                maybeCons click
                [ class ("side " ++ cls ++ " " ++ ownercls)
                , title ((toString elem.location.row) ++ ", " ++ (toString elem.location.column))
                ]
            ) []

sideElement: Model -> Int -> Int -> SideDirection -> Html Msg
sideElement model row col direction =
    let e = findSide model row col
    in
        case e of
            Just val ->
                side model val direction
            Nothing ->
                Html.text ""

mapPlayerToLabel s1 = case s1.owner of
        None -> ""
        Player1 -> "1"
        Player2 -> "2"

mapPlayerToClass s1 = case s1.owner of
        None -> "no-owner"
        Player1 -> "owner-player1"
        Player2 -> "owner-player2"

square: Model -> Int -> Int -> Html Msg
square model row column =
    let
        s = findSquare model row column
    in
        case s of
        Just val ->
            div [class ("square " ++ (mapPlayerToClass val))]
                --[ text ((mapPlayerToLabel val) ++ (toString val.location.row) ++ " " ++ (toString val.location.column)) ]
                [ text (mapPlayerToLabel val) ]
        Nothing ->
            div [class "square"] [ text "x" ]


cornerList: Int -> List (Html Msg)
cornerList columns =
        List.foldr (\column accum -> (corner) :: accum) [ ] (seq columns)

sideList: Model -> Int -> Int -> SideDirection -> List (Html Msg)
sideList model row columns direction =
        List.foldr (\column accum -> (sideElement model row column direction) :: accum) [ ] (seq columns)

squareList: Model -> Int -> Int -> List (Html Msg)
squareList model row columns =
        List.foldr (\column accum -> (square model row column) :: accum ) [ ] (seq columns)

sideRow: Model -> Int -> Int -> Html Msg
sideRow model row columns =
    div [class "row" ]
        (zipper (cornerList columns) (sideList model (2*row) columns Horizontal)
            |> List.take (2 * columns - 1)
        )

squareRow: Model -> Int -> Int -> Html Msg
squareRow model row columns =
    div [class "row" ]
        ( zipper (sideList model (2 * row + 1) columns Vertical) (squareList model row columns)
            |> List.take (2 * columns - 1)
        )

toRows: Model -> List (Html Msg)
toRows model =
    let cols = model.columns + 1
        rows = model.rows + 1
    in
        List.foldr (\row accum ->
          List.singleton (sideRow model row cols)
          ++
          List.singleton (squareRow model row cols)
          ++  accum
        ) [] (seq model.rows)
        ++
        List.singleton (sideRow model (model.rows) cols)


toPlayerArea: Model -> List (Html Msg)
toPlayerArea model =
    [
    ]

gameOver: Model -> Html Msg
gameOver model =
    let winner = if model.score.player1 >= model.score.player2 then
            div [] [text "Player 1 wins"]
         else
             div [] [text "Player 2 wins"]

        newGameButton = if model.gameOver then
            button [onClick NewGame ] [text "New Game"]
        else
            span [] [text ""]
    in
        if model.gameOver then
            div [class "game-over"][
                div [] [text "Game Over"]
                , winner
                , newGameButton
            ]
    else
        div [][]

gameBoard: Model -> Html Msg
gameBoard model =
    let
        game = (div [class "rows"] (toRows model))
    in
        div []
            [ game ]

gameStatus: Model -> Html Msg
gameStatus model =
    div[ id "game-status"]
        (List.singleton (gameOver model))

showPlayer: String -> Int -> Bool -> String -> Html Msg
showPlayer label score isCurrentPlayer playerclass =
    let cls = if isCurrentPlayer then "current-player" else ""
    in
        div [class (classList ["player-status",cls,playerclass])] [text (label ++ toString score)]

players: Model -> List (Html Msg)
players model =
    [
        (showPlayer (model.namePlayer1 ++ ": ") model.score.player1 (model.currentPlayer == Player1) "player1")
        , (showPlayer (model.namePlayer2 ++ ": ") model.score.player2 (model.currentPlayer == Player2) "player2")
    ]


-- Control Panel
controlPanel: Model -> Html Msg
controlPanel model =
    if model.showForm then
        form model
    else
        div [][]

numericInput: String -> String -> Int -> Int -> (String -> Msg) -> Html Msg
numericInput idStr lbl val maxValue msg =
    div[class "numeric-input"] [
      label [for idStr ] [text lbl]
      ,span [] [text (toString val)]
      ,input [
        id idStr
        , value (toString val)
        , type_ "range"
        , Html.Attributes.min "2"
        , Html.Attributes.max (toString maxValue)
        ,onInput msg] []
      ]

form: Model -> Html Msg
form model =
    div [class "control-panel"] [
        div[] [
            label [for "namePlayer1" ] [text "Player 1 Name: "]
            ,input [ id "namePlayer1",  value model.namePlayer1, onInput UpdateNamePlayer1] []
            ]
        ,div[] [
            label [for "namePlayer2" ] [text "Player 2 Name: "]
            ,input [ id "namePlayer2",  value model.namePlayer2, onInput UpdateNamePlayer2] []
            ]
        , numericInput "rows" "Rows" model.rows model.maxRows UpdateRows
        , numericInput "cols" "Cols" model.columns model.maxColumns UpdateColumns
        ]

----------------------------
-- tools
----------------------------

view : Model -> Html Msg
view model =
    div[ id "app" ]
        [
        gameStatus model
        , controlPanel model
        , div [class "game" ]
            (List.singleton (gameBoard model)
            ++
            players model
            )
        ]
