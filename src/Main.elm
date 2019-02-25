module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class)
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


type alias Model =
    { squares : List (List Cell)
    }


init =
    { squares = List.repeat constants.width <| List.repeat constants.height EmptyCell
    }


type Msg
    = Blacken Int Int
    | Empty Int Int


changeCellAt : List (List Cell) -> Int -> Int -> Cell -> List (List Cell)
changeCellAt grid xIndex yIndex newCell =
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


update msg model =
    case msg of
        Blacken xIndex yIndex ->
            { model
                | squares = changeCellAt model.squares xIndex yIndex BlackCell
            }

        Empty xIndex yIndex ->
            { model
                | squares = changeCellAt model.squares xIndex yIndex EmptyCell
            }


cellRender : Int -> Int -> Cell -> Html Msg
cellRender xIndex yIndex cell =
    case cell of
        EmptyCell ->
            td [ class "emptycell", onClick (Blacken xIndex yIndex) ] []

        BlackCell ->
            td [ class "blackcell", onClick (Empty xIndex yIndex) ] []

        Just c ->
            td [ class "fillcell" ] [ text c ]


view model =
    div
        []
        [ Html.node "style" [] [ text css ]
        , table
            []
            (model.squares
                |> List.indexedMap
                    (\x row ->
                        tr []
                            (row |> List.indexedMap (cellRender x))
                    )
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
}

td.blackcell {
    background-color: #222;
}
"""
