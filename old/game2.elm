import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Array2D

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL

type Owner = None | Player1 | Player2
type Direction = Horizontal | Vertical

type alias Corner =
    {
        owner: Owner
    }

type alias Side =
    {
        owner: Owner
        , direction: Direction
        , row: Int
        , col: Int
    }

type alias Square =
    {
        owner: Owner
    }

type Element =
    ElementCorner Corner
    | ElementSide Side
    | ElementSquare Square

type alias Rows = List Row

type alias Row = List Element

type alias Model =
  {
    clickCount: Int
    , board: Rows
    , rows: Int
    , maxRows: Int
    , columns: Int
    , maxColumns: Int
  }


sideRow: Int -> Row
sideRow columns =
    let
        rng =  List.range 0 (columns-1)

        flattened = List.concatMap (\l ->
            [
                ElementCorner { owner=None }
                ,ElementSide { owner=None, direction = Horizontal, row = -1, col = -1 }
            ]) rng
    in
        List.append flattened [ ElementCorner { owner=None } ]

squareRow: Int -> Row
squareRow columns =
    let
        rng =  List.range 0 (columns-1)

        flattened = List.concatMap (\l ->
            [
                ElementSide { owner=None, direction=Vertical, row = -1, col = -1 }
                ,ElementSquare { owner=None }
            ]) rng
    in
        List.append flattened [ ElementSide {owner=None, direction=Vertical, row = -1, col = -1 } ]

newBoard: Int ->  Int -> List Row
newBoard rows columns =
    let
        rng = List.range 0 (rows-1)

        flattened = List.concatMap (\ r -> [
            sideRow columns
            , squareRow columns
        ]) rng
    in
        List.append flattened [ sideRow columns ]


-- INIT
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  {
    clickCount=0
    , board = newBoard 3 4
    , rows= 3
    , maxRows= 10
    , columns=4
    , maxColumns=10
  }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE
type Msg =
    NullMsg
    | HelloWorld
    | UpdateRows String
    | UpdateColumns String
    | CreateBoard
    | ClickSide (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NullMsg ->
        (model, Cmd.none)

    HelloWorld ->
        (model, Cmd.none)

    UpdateRows rows ->
        ( { model | rows = setIfInRange rows model.rows 2 model.maxRows } , Cmd.none )

    UpdateColumns columns ->
        ( {model | columns = setIfInRange columns model.columns 2 model.maxColumns } , Cmd.none)

    CreateBoard ->
        ( { model | board = newBoard model.rows model.columns }, Cmd.none)

    ClickSide (row, column) ->
        ( { model | board = doClickSide model row column }, Cmd.none)


updateRow: Row -> Int -> Row
updateRow boardrow columnIndex =
    let
        assign index element =
            if index == columnIndex then
                ({ element | owner = Player1 })
            else
                ( element )
    in
        List.indexedMap assign boardrow

doClickSide: Model -> Int -> Int -> Rows
doClickSide model rowindex columnindex =
    let
        assign index boardrow =
            if index == rowindex then
                updateRow (boardrow columnindex)
            else
                ( boardrow )

        newList = List.indexedMap assign model.board
    in
        newList

-- VIEW

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
--------------------------------------------------------------------------------------

corner =
    div [class "corner" ] []

side: Side -> Direction -> Html Msg
side elem direction =
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

        div [class ("side " ++ cls ++ " " ++ ownercls),
         onClick (ClickSide (3, 4))
         ] []

square s =
    div [class "square"] []

horizontalSideRow: Model -> Int -> Int ->  Html Msg
horizontalSideRow model row columns =
    let c = List.range 0 columns
    in
        List.indexedMap (\column _ -> )


toRows: Model -> Html Msg
toRows model =
    let r = List.range 0 rows
    in
        List.indexedMap (\row _ ->
            horizontalRow
            squareRow
            .... horizontalRow
            toRow model row model.columns) r

toRow: Model -> Int -> Int -> Html Msg
toRow row columns =
    let c = List.range 0 columns
    in
        List.indexedMap(\column ->
            corner
            side model Horizontal row column
            <side>
        )

toSide: Model -> Direction -> Int -> Int -> Html Msg
toSide model direction row column =
    corner
    side direction elem
    ...


leftSide: Model -> Html Msg
leftSide model =
    div [class "rows"]
        (rows model.board)


--corner:  ElementCorner -> Html Msg

--side: ElementSide -> Html Msg

element : Element -> Html Msg
element elem =
    case elem of
        ElementCorner c ->
            corner
        ElementSide s ->
            side s
        ElementSquare s ->
            square s

row : Row -> List (Html Msg)
row r =
    List.map (\elem -> element elem) r

rowdiv r =
    div [class "row" ] (row r)

rows : Rows -> List (Html Msg)
rows board =
    List.map (\r -> (rowdiv r)) board

------ right side -----------------------

rightSide: Model -> Html Msg
rightSide model =
    div [] [
        div [] [text (toString model.clickCount) ]
        ,form model
        , display model
        ]


numericInput: String -> String -> Int -> Int -> (String -> Msg) -> Html Msg
numericInput idStr lbl val maxValue msg =
    div[] [
      label [for idStr ] [text lbl]
      ,input [
        id idStr
        , value (toString val)
        , type_ "number"
        , Html.Attributes.max (toString maxValue)
        ,onInput msg] []
      ]

form: Model -> Html Msg
form model =
    div [] [
            numericInput "rows" "Rows" model.rows model.maxRows UpdateRows
            , numericInput "cols" "Cols" model.columns model.maxColumns UpdateColumns
        ]
display: Model -> Html Msg
display model =
    div [] [
    div [] [text ("Dimensions " ++ toString model.rows ++ " x " ++ toString model.columns) ]

    ]

view : Model -> Html Msg
view model =
    div[ id "app" ]
        [ stylesheet
        , leftSide model
        , rightSide model
        ]



----------------------------
-- tools
----------------------------
setIfInRange: String -> Int -> Int -> Int -> Int
setIfInRange newVal currentValue minValue maxValue =
    let nv = Result.withDefault currentValue (String.toInt newVal)
    in
    if minValue <= nv && nv <= maxValue then
        nv
    else
        currentValue

