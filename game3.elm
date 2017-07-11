import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Array2D

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL

type Owner = None | Player1 | Player2

type alias Location =
    {
    row: Int
    , column : Int
    }

type alias Square =
    {
        location: Location
        , owner: Owner
    }

type alias Side =
    {
        location: Location
        , owner: Owner
    }

type alias Board =
    {
        squares: List Square
        , sides: List Side
    }

type alias Score =
    {
        player1: Int
        , player2: Int
    }

type alias Model =
  {
    board: Board
    , score: Score
    , gameOver: Bool
    , currentPlayer: Owner
    , currentPlayerCompletedSquare: Bool
    , rows: Int
    ,columns: Int
  }

seq: Int -> List Int
seq max =
    List.range 0 (max-1)

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

newBoard: Int ->  Int -> Board
newBoard rows columns =

    let squares = sqGrid rows columns
        sides =   sideGrid ((2 * rows) + 1) (columns + 1)
    in
        Board squares  sides

-- INIT
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  {
     board = newBoard 3 4
     , score = Score 0 0
     , gameOver = False
     , currentPlayer = Player1
     , currentPlayerCompletedSquare = False
     , rows = 3
     , columns = 4
  }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE
type Msg =
    CreateBoard
    | ClickSide (Side)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CreateBoard ->
        ( { model | board = newBoard 3 4 }, Cmd.none)

    ClickSide (side) ->
        ( updateTurn model side, Cmd.none)


updateTurn: Model -> Side -> Model
updateTurn model side =
    resetPlayerCompletedSquare model
    |> updateSide side
    |> updateSquares
    |> updatePlayer
    |> checkGameOver

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
    in

        div [class ("side " ++ cls ++ " " ++ ownercls)
        , onClick (ClickSide elem)
        , title ((toString elem.location.row) ++ ", " ++ (toString elem.location.column))
         ] []

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
        None -> ""
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
        div [] [
            text ((toString model.currentPlayer) ++ ", it's your turn!")
            , div [class "score"] [
                div [] [text ("Player1: " ++ toString model.score.player1)]
                , div [] [text ("Player2: " ++ toString model.score.player2)]
            ]
         ]
    ]

gameOver: Model -> Html Msg
gameOver model =
    let winner = if model.score.player1 >= model.score.player2 then
            div [] [text "Player 1 wins"]
         else
             div [] [text "Player 2 wins"]
    in
        if model.gameOver then
            div [class "game-over"][
                div [] [text "Game Over"]
                , winner
            ]
    else
        div [][]

leftSide: Model -> Html Msg
leftSide model =
    let playerArea = (div [class "player-area"] (toPlayerArea model))
        gameOverDiv = gameOver model
        game = (div [class "rows"] (toRows model))
    in
        div []
            (
            (List.singleton playerArea)
            ++
            (List.singleton gameOverDiv)
            ++
            (List.singleton game)
            )

view : Model -> Html Msg
view model =
    div[ id "app" ]
        [ stylesheet
        , leftSide model
        ]
