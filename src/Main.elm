module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Browser.Events
import Debug
import Html exposing (Html, h1, button, div, table, td, text, tr)
import Html.Attributes exposing (class, classList, for, id, name, type_, value)
import Html.Events exposing (onClick, onMouseEnter)
import Json.Decode as Decode
import Platform.Sub as Sub exposing (Sub)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


constants =
    { width = 15
    , height = 15
    }


type Cell
    = BlackCell
    | EmptyCell
    | FillCell String


type SymmetryMode
    = Rotate180
    | NoSymmetry


type alias Model =
    { squares : List (List Cell)
    , symmetry : SymmetryMode
    , overX : Int
    , overY : Int
    }


emptyGrid =
    List.repeat constants.height <| List.repeat constants.width EmptyCell


init : () -> ( Model, Cmd Msg )
init _ =
    ( { squares = emptyGrid
      , symmetry = Rotate180
      , overX = 0
      , overY = 0
      }
    , Cmd.none
    )


type Msg
    = Blacken Int Int
    | Empty Int Int
    | ChangeSymmetry SymmetryMode
    | Write String
    | WriteEmpty
    | Over Int Int 


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Write (char |> String.fromChar |> String.toUpper)

        _ ->
            WriteEmpty


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyPress keyDecoder


changeCellAt : Int -> Int -> Cell -> List (List Cell) -> List (List Cell)
changeCellAt xIndex yIndex newCell grid =
    grid
        |> List.indexedMap
            (\x row ->
                row
                    |> List.indexedMap
                        (\y cell ->
                            if x == xIndex && y == yIndex then
                                newCell

                            else
                                cell
                        )
            )


symmetry180Change : Int -> Int -> Cell -> List (List Cell) -> List (List Cell)
symmetry180Change x y newCell grid =
    changeCellAt (constants.width - x - 1) (constants.height - y - 1) newCell grid


updateGrid : Int -> Int -> Cell -> Model -> Model
updateGrid x y newCell model =
    { model
        | squares =
            case newCell of
            FillCell _ ->
                model.squares
                    |> changeCellAt x y newCell

            _ ->
                case model.symmetry of
                    NoSymmetry ->
                        model.squares
                            |> changeCellAt x y newCell

                    Rotate180 ->
                        model.squares
                            |> changeCellAt x y newCell
                            |> symmetry180Change x y newCell
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blacken x y ->
            ( model |> updateGrid x y BlackCell, Cmd.none )

        Empty x y ->
            ( model |> updateGrid x y EmptyCell, Cmd.none )

        ChangeSymmetry newSymmetry ->
            ( { model | symmetry = newSymmetry }, Cmd.none )

        Write string ->
            ( model |> updateGrid model.overX model.overY (FillCell string), Cmd.none )

        Over x y ->
            ({ model | overX = x, overY = y }, Cmd.none)

        WriteEmpty ->
            ( model |> updateGrid model.overX model.overY EmptyCell, Cmd.none )


cellRender : Model -> Int -> Int -> Cell -> Html Msg
cellRender model x y cell =
    case cell of
        BlackCell ->
            td [ class "blackCell", onClick (Empty x y), onMouseEnter (Over x y)] []

        EmptyCell ->
            td [ class "emptyCell", onClick (Blacken x y), onMouseEnter (Over x y)] []

        FillCell c ->
            td [ class "fillCell", onMouseEnter (Over x y)] [ text c ]


view model =
    div []
        [ Html.node "style" [] [ text css ]
        , h1 [] [ text "Crossword Helper"]
        , table []
            (model.squares
                |> List.indexedMap
                    (\x row ->
                        tr []
                            (row |> List.indexedMap (cellRender model x))
                    )
            )
        , div []
            [ div [ onClick (ChangeSymmetry NoSymmetry), classList [ ( "radio", True ), ( "checked", model.symmetry == NoSymmetry ) ] ] [ text "No symmetry" ]
            , div [ onClick (ChangeSymmetry Rotate180), classList [ ( "radio", True ), ( "checked", model.symmetry == Rotate180 ) ] ] [ text "Rotational symmetry 180º" ]
            ]
        ]


css : String
css =
    """
body {
    display: flex;
    justify-content: center; 
    font-family: sans-serif
}

h1 {
    text-align: center;
}

table, tr, td {
  border-collapse: collapse;
}

td {
    border: 1px solid black;
    width: 2.2em;
    height: 2.2em;
    cursor: pointer;
    display: table-cell;
    vertical-align: center;
    text-align: center;
    box-sizing: border-box;
    font-size: 1.2em;
}

td:hover {
    border-width: 2px;
}

.blackCell {
    background-color: #222;
}

.greenCell {
    background-color: #dfd;
}

.redCell {
    background-color: #fdd;
}

input[type=radio] {
    display: none;
}

.radio {
    cursor: pointer;
    display: inline-block;
    padding: 0.5em;
    margin-top: 1em;
    margin-right: 1em;
    border: 1px solid #444;
}

.radio.checked {
    background-color: #eee;
}
"""
