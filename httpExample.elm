import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (..)
import Json.Decode exposing (list, int, string, float, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

main =
  Html.program { init=init, view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Topic =
  { topic_id : Int
  , iid : Int
  , group_id : Int
  , name : String
  , count: Int
  , err : String
  }

type alias Model =
  { topics : List Topic
  , err : String
  }

-- INIT
init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  { topics=[]
  , err = ""
  }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

type Msg =
    GetStr
    | GetHttp
    | NewTopic (Result Http.Error (List Topic))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetStr ->
        case Json.Decode.decodeString (Json.Decode.at ["topics"] topicsDecoder)
        --"""{ "topics": [{"topic_id": 203, "iid": 264, "group_id": 2, "name": "How To...","count": 1 },{"topic_id": 203, "iid": 264, "group_id": 2, "name": "Me","count": 1 }]} """
        """{ "topics": []} """
        of
            Ok result ->
                ( { model | topics = result}, Cmd.none )
            Err msg ->
                ( { model | err = "Error"} , Cmd.none )

    GetHttp ->
        (model, getHttp)

    NewTopic (Ok topics) ->
        ({ model | topics = topics}, Cmd.none)

    NewTopic (Err err) ->
          ({model | err = toString err}, Cmd.none)


-- VIEW

view_topic : Topic -> Html Msg
view_topic topic =
        div [] [text ("xx" ++ topic.name)]

view : Model -> Html Msg
view model =
    div[] [
        div [] [text (toString (length model.topics))]
        ,div [] (List.map view_topic model.topics)
        , div [] [text (model.err)]
        , button [onClick GetStr] [text "load" ]
        , button [onClick GetHttp] [text "load" ]
        ]

-----------------------------------------------------

getHttp : Cmd Msg
getHttp =
  let
    url =
      "https://api2.libanswers.com/1.0/topics?iid=264&sort=name"
  in
    Http.send NewTopic (Http.get url topicsDecoder)


topicsDecoder : Decoder (List Topic)
topicsDecoder = Json.Decode.list topicDecoder

topicDecoder : Decoder Topic
topicDecoder =
  decode Topic
    |> Json.Decode.Pipeline.required "topic_id" int
    |> Json.Decode.Pipeline.required "iid" int
    |> Json.Decode.Pipeline.required "group_id" int
    |> Json.Decode.Pipeline.required "name" string
    |> Json.Decode.Pipeline.required "count" int
    |> Json.Decode.Pipeline.hardcoded ""

