import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Array2D

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL

type Owner = None | Player1 | Player2

type alias Corner =
    {
    }

type alias Square =
    {
        owner : Owner
    }

type Direction = Horizontal | Vertical

type alias Line =
    {
        owner: Owner
        , direction: Direction
    }

type Element =
      ElementSquare Square
    | ElementLine Line
    | ElementCorner Corner

--type Row = List Element

type Rows a = Array2D a

type alias Model =
  {
    gameboard: Element
    , rows: Int
    , maxRows: Int
    , columns: Int
    , maxColumns: Int
  }

-- INIT
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  {
  gameboard = createBoardRowsColumns 3 4
  , rows= 3
  , maxRows= 10
  , columns=4
  , maxColumns=10
  }

createSquareColumn : List Element
createSquareColumn =
    [
        ElementLine { owner = None, direction=Vertical }
        , ElementSquare { owner = None }
    ]

--createLineColumn =
--    [
--        ElementCorner
--        , ElementLine { owner = None, direction=Horizontal }
--    ]

--createSquareRow columns =
--    List.map (\ column -> createSquareColumn column) List.range(0,columns) :: [ ElementLine { owner = None, direction=Vertical }]

--createLineRow columns =
--    List.map createLineColumn List.range(0,columns) :: [ ElementCorner ]

--createRow: List Element
--createRow columns =
--    createLineRow columns :: createSquareRow :: columns

--createBoardRowsColumns: List (List Element)
createBoardRowsColumns rows columns =
    ElementCorner
    --let
    --    l = List.range(0,rows)
    --in
    --    List.map (\row -> createRow row) l :: createLineRow columns

createBoard: Model -> Element
createBoard model =
    createBoardRowsColumns model.rows model.columns

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
        ( { model | gameboard = Corner }, Cmd.none)
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

leftSide: Model -> Html Msg


leftSide model =
    div [class "gameboard"] []

corner: Html Msg
corner =
    div [class "corner" ] []

horizontalSide: Html Msg
horizontalSide =
    div [class "side horizontal"][]

topRowElementList: Int -> List (Html Msg)
topRowElementList boxes =
    List.append (List.intersperse horizontalSide (List.repeat boxes corner)) [ corner ]

topRow model =
    div [] (topRowElementList model.columns)

------ right side -----------------------

rightSide: Model -> Html Msg
rightSide model =
    div [] [
        form model
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

