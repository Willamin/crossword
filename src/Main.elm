module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Browser.Events
import Debug
import Html exposing (Html, button, div, h1, input, li, pre, table, td, text, tr, ul)
import Html.Attributes exposing (class, classList, contenteditable, for, id, name, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
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
    , over : Position
    }


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


emptyGrid =
    List.repeat constants.width <| List.repeat constants.height EmptyCell


init : () -> ( Model, Cmd Msg )
init _ =
    ( { squares = emptyGrid
      , symmetry = Rotate180
      , over = NoCoord
      }
    , Cmd.none
    )


type Position
    = Coord Int Int
    | NoCoord


type Msg
    = Blacken Position
    | Empty Position
    | ChangeSymmetry SymmetryMode
    | Write String
    | WriteEmpty
    | Over Position
    | ParsePuzzle String


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


getCellAt : Int -> Int -> List (List Cell) -> Cell
getCellAt x y grid =
    grid
        |> Array.fromList
        |> Array.get x
        |> Maybe.withDefault []
        |> Array.fromList
        |> Array.get y
        |> Maybe.withDefault EmptyCell


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


updateGrid : Position -> Cell -> Model -> Model
updateGrid pos newCell model =
    case pos of
        NoCoord ->
            model

        Coord x y ->
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
                , over =
                    if newCell == BlackCell then
                        NoCoord

                    else
                        model.over
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blacken pos ->
            ( model |> updateGrid pos BlackCell, Cmd.none )

        Empty pos ->
            ( model |> updateGrid pos EmptyCell, Cmd.none )

        ChangeSymmetry newSymmetry ->
            ( { model | symmetry = newSymmetry }, Cmd.none )

        Write string ->
            ( model |> updateGrid model.over (FillCell string), Cmd.none )

        Over pos ->
            ( { model | over = pos }, Cmd.none )

        WriteEmpty ->
            ( model |> updateGrid model.over EmptyCell, Cmd.none )

        ParsePuzzle string ->
            ( { model | squares = textToGrid string }, Cmd.none )



-- ( model, Cmd.none )


cellIsAtWordHead : List (List Cell) -> Position -> Bool
cellIsAtWordHead grid pos =
    case pos of
        NoCoord ->
            False

        Coord x y ->
            x
                == 0
                || y
                == 0
                || getCellAt (x - 1) y grid
                == BlackCell
                || getCellAt x (y - 1) grid
                == BlackCell


cellRender : Model -> Int -> Int -> Cell -> Html Msg
cellRender model x y cell =
    let
        pos =
            Coord x y
    in
    case cell of
        BlackCell ->
            td [ class "blackCell", onClick (Empty pos), onMouseEnter (Over NoCoord) ] []

        EmptyCell ->
            td [ class "emptyCell", classList [ ( "greenCell", cellIsAtWordHead model.squares pos ) ], onClick (Blacken pos), onMouseEnter (Over <| pos) ] []

        FillCell c ->
            td [ class "fillCell", classList [ ( "greenCell", cellIsAtWordHead model.squares pos ) ], onMouseEnter (Over <| pos) ] [ text c ]


gridToText : List (List Cell) -> String
gridToText grid =
    grid
        |> List.map
            (\row ->
                row
                    |> List.map
                        (\cell ->
                            case cell of
                                BlackCell ->
                                    "!"

                                EmptyCell ->
                                    "-"

                                FillCell c ->
                                    c
                        )
                    |> String.concat
            )
        |> String.concat


textToGrid : String -> List (List Cell)
textToGrid string =
    string
        |> String.toList
        |> List.take (constants.width * constants.height)
        |> List.map
            (\c ->
                case c of
                    '-' ->
                        EmptyCell

                    '!' ->
                        BlackCell

                    s ->
                        FillCell <| String.fromChar s
            )
        |> split 15


view model =
    div []
        [ Html.node "style" [] [ text css ]

        -- , h1 [] [ text "Crossword Helper" ]
        , table [ onMouseLeave (Over NoCoord) ]
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
        , Html.textarea [ onInput ParsePuzzle ]
            [ text <| gridToText model.squares
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

textarea {
    margin-top: 1.5em;
    font-family: monospace;
    width: 100%;
    height: 5em;
    border: 1px solid #de235c;
    border-radius: 4px;
    white-space: pre-wrap;
    word-break: break-all;
    color: #de235c;
    background-color: #f7f7f9;
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
