module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, for, id, name, type_, value)
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
    | None


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
            model.squares
                |> changeCellAt x y newCell
                |> symmetry180Change x y newCell
    }


update msg model =
    case msg of
        Blacken x y ->
            model |> updateGrid x y BlackCell

        Empty x y ->
            model |> updateGrid x y BlackCell


cellRender : Int -> Int -> Cell -> Html Msg
cellRender xIndex yIndex cell =
    case cell of
        EmptyCell ->
            td [ class "emptycell", onClick (Blacken xIndex yIndex) ] []

        BlackCell ->
            td [ class "blackcell", onClick (Empty xIndex yIndex) ] []

        Just c ->
            td [ class "fillcell" ] [ text c ]


radioGroup : List ( String, String, String ) -> List (Html Msg)
radioGroup listofThings =
    listofThings
        |> List.map
            (\( group, value_, label ) ->
                [ Html.input [ type_ "radio", name group, value value_, id value_ ] []
                , Html.label [ for value_, class "forRadio" ] [ text label ]
                ]
            )
        |> List.foldr (++) []


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
            (radioGroup
                [ ( "symmetry", "symmetryNone", "No symmetry" )
                , ( "symmetry", "symmetryRotate180", "Rotational symmetry 180º" )
                ]
            )
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
}

td.blackcell {
    background-color: #222;
}

input[type=radio] {
    display: none;
}

label.forRadio {
    cursor: pointer;
    display: inline-block;
    padding: 0.5em;
    margin-top: 1em;
    margin-right: 1em;
    border: 1px solid #444;
}

input[type=radio]:checked + label.forRadio {
    background-color: lightgray;
}
"""
