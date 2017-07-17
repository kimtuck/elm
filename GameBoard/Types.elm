module GameBoard.Types exposing (..)

type alias Model = Board


type Msg =
    CreateBoard
   | UpdateRows String
   | UpdateColumns String
   | ClickSide (Side)
   | NewGame

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
