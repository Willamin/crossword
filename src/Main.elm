module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, classList, for, id, name, type_, value)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


constants =
    { width = 15
    , height = 15
    }


type Cell
    = BlackCell
    | EmptyCell
    | Just String


type SymmetryMode
    = Rotate180
    | NoSymmetry


type alias Model =
    { squares : List (List Cell)
    , symmetry : SymmetryMode
    }


emptyGrid =
    List.repeat constants.height <| List.repeat constants.width EmptyCell


init =
    { squares = emptyGrid
    , symmetry = Rotate180
    }


type Msg
    = Blacken Int Int
    | Empty Int Int
    | ChangeSymmetry SymmetryMode


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
            case model.symmetry of
                NoSymmetry ->
                    model.squares
                        |> changeCellAt x y newCell

                Rotate180 ->
                    model.squares
                        |> changeCellAt x y newCell
                        |> symmetry180Change x y newCell
    }


update msg model =
    case msg of
        Blacken x y ->
            model |> updateGrid x y BlackCell

        Empty x y ->
            model |> updateGrid x y EmptyCell

        ChangeSymmetry newMode ->
            { model | symmetry = newMode }


cellRender : Int -> Int -> Cell -> Html Msg
cellRender xIndex yIndex cell =
    case cell of
        BlackCell ->
            td [ class "blackCell", onClick (Empty xIndex yIndex) ] []

        EmptyCell ->
            td [ classList [ ( "emptyCell", True ), ( "redCell", False ), ( "greenCell", False ) ], onClick (Blacken xIndex yIndex) ] []

        Just c ->
            td [ classList [ ( "fillCell", True ), ( "redCell", False ), ( "greenCell", False ) ] ] [ text c ]


view model =
    div []
        [ Html.node "style" [] [ text css ]
        , table []
            (model.squares
                |> List.indexedMap
                    (\x row ->
                        tr []
                            (row |> List.indexedMap (cellRender x))
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
}

table, tr, td {
  border: 1px solid black;
  border-collapse: collapse;
}

td {
    width: 2em;
    height: 2em;
    cursor: pointer;
    display: table-cell;
    vertical-align: center;
    text-align: center;
}

td.blackCell {
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
    background-color: lightgray;
}
"""
