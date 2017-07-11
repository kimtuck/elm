import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL

type alias Model =
    { count: Int
    , content: String
    , name : String
    , password : String
    , passwordAgain : String
    , dieFace: Int
    ,topic : String
    , gifUrl : String
    }

-- INIT
init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  { count = 0,
    content = ""
    , name =""
    ,password = ""
    , passwordAgain = ""
    ,dieFace = 0
    ,topic = "Cats"
    , gifUrl = "waiting.gif"
  }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

type Msg =
      Increment
    | Decrement
    | Reset
    | Change String
    | Name String
    | Password String
    | PasswordAgain String
    | Roll
    | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({ model | count = model.count + 1
      }, Cmd.none)

    Decrement ->
      ({ model | count = model.count - 1
      }, Cmd.none)

    Reset ->
        ({ model | count = 0
            , content = ""
        }, Cmd.none)

    Change str ->
        ({ model | content = str }, Cmd.none)

    Name str ->
        ({ model | name = str }, Cmd.none)

    Password str ->
        ({ model | password = str }, Cmd.none)

    PasswordAgain str ->
        ({ model | passwordAgain = str }, Cmd.none)

    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      ({ model | dieFace = newFace}, Cmd.none)

-- VIEW

incr: Model -> Html Msg
incr model =
  div []
    [ button [ onClick Decrement ] [ text "--" ]
    , div [] [ text (toString model.count) ]
    , button [ onClick Increment ] [ text "++" ]
    ]

reset =
    div []
     [
        button [ onClick Reset ] [ text "Reset" ]
     ]

inp model =
     div []
     [ input [ placeholder "Text to reverse", onInput Change, value model.content ] []
     , div [] [ text (String.reverse model.content) ]
     ]

form model = div []
           [ input [ type_ "text", placeholder "Name", onInput Name ] []
           , input [ type_ "password", placeholder "Password", onInput Password ] []
           , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
           , viewValidation model
           ]
die model = div []
                [ h1 [] [ text (toString model.dieFace) ]
                , button [ onClick Roll ] [ text "Roll" ]
                ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password == model.passwordAgain then
        ("green", "OK")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]


view : Model -> Html Msg
view model =
     div []
     [ incr model
     , reset
     , inp model
     , form model
     , die model
     ]
