module Main exposing (Cell(..), Model, Msg(..), cellRender, changeCellAt, css, init, main, update, view)

import Array
import Browser
import Browser.Events
import Debug
import Html exposing (Html, button, div, h1, input, li, pre, table, td, text, tr, ul)
import Html.Attributes exposing (attribute, class, classList, contenteditable, for, id, name, type_, value)
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
    | HorizontalSymmetry
    | VerticalSymmetry
    | NoSymmetry


type alias Model =
    { squares : List (List Cell)
    , symmetry : SymmetryMode
    , over : Position
    , highlightClueStarts : Bool
    , indicateClueNumbers : Bool
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
      , highlightClueStarts = False
      , indicateClueNumbers = True
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
    | ChangeHighlightClueStarts Bool
    | ChangeClueNumberIndication Bool


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


symmetryHorizontalChange : Int -> Int -> Cell -> List (List Cell) -> List (List Cell)
symmetryHorizontalChange x y newCell grid =
    changeCellAt (constants.width - x - 1) y newCell grid


symmetryVerticalChange : Int -> Int -> Cell -> List (List Cell) -> List (List Cell)
symmetryVerticalChange x y newCell grid =
    changeCellAt x (constants.height - y - 1) newCell grid


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

                                HorizontalSymmetry ->
                                    model.squares
                                        |> changeCellAt x y newCell
                                        |> symmetryHorizontalChange x y newCell

                                VerticalSymmetry ->
                                    model.squares
                                        |> changeCellAt x y newCell
                                        |> symmetryVerticalChange x y newCell
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

        ChangeHighlightClueStarts newHighlight ->
            ( { model | highlightClueStarts = newHighlight }, Cmd.none )

        ChangeClueNumberIndication newIndication ->
            ( { model | indicateClueNumbers = newIndication }, Cmd.none )



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


collapseStringifiedHelper : String -> String
collapseStringifiedHelper input =
    case input of
        "!" ->
            " "

        "-" ->
            " "

        c ->
            c


collapseToWords : String -> String
collapseToWords string =
    string
        |> String.split ""
        |> List.map collapseStringifiedHelper
        |> String.join ""
        |> String.words
        |> String.join " "


positionFromLinePos : Int -> Position
positionFromLinePos i =
    if i < 0 || i > constants.width * constants.height then
        NoCoord

    else
        Coord (i // constants.width) (i |> remainderBy constants.height)


currentClue : List (List Cell) -> Int -> Int
currentClue grid index =
    let
        foo =
            grid
                |> gridToList
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\( cellNumber, cell ) ->
                        case cell of
                            BlackCell ->
                                False

                            _ ->
                                cellIsAtWordHead grid (positionFromLinePos cellNumber)
                    )
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\( clue, ( idx, cell ) ) ->
                        index == idx
                    )
                |> List.head
    in
    case foo of
        Just ( clue, ( idx, cell ) ) ->
            clue + 1

        Nothing ->
            0


clueNumber : List (List Cell) -> Position -> String
clueNumber grid pos =
    case pos of
        NoCoord ->
            ""

        Coord x y ->
            if cellIsAtWordHead grid pos then
                String.fromInt <| currentClue grid (x * constants.width + y)

            else
                ""


cellRender : Model -> Int -> Int -> Cell -> Html Msg
cellRender model x y cell =
    let
        pos =
            Coord x y
    in
    case cell of
        BlackCell ->
            td [ class "blackCell", onClick (Empty pos), onMouseEnter (Over NoCoord) ]
                []

        EmptyCell ->
            td
                [ class "emptyCell"
                , classList [ ( "greenCell", model.highlightClueStarts && cellIsAtWordHead model.squares pos ) ]
                , onClick (Blacken pos)
                , onMouseEnter (Over <| pos)
                ]
                [ div [ class "number" ]
                    [ text <|
                        if model.indicateClueNumbers then
                            clueNumber model.squares pos

                        else
                            ""
                    ]
                , div [] []
                ]

        FillCell c ->
            td
                [ class "fillCell"
                , classList [ ( "greenCell", model.highlightClueStarts && cellIsAtWordHead model.squares pos ) ]
                , onMouseEnter (Over <| pos)
                ]
                [ div [ class "number" ]
                    [ text <|
                        if model.indicateClueNumbers then
                            clueNumber model.squares pos

                        else
                            ""
                    ]
                , div [ class "fill" ] [ text c ]
                ]


gridToList : List (List Cell) -> List Cell
gridToList grid =
    grid |> List.concat


listToGrid : List Cell -> List (List Cell)
listToGrid list =
    list |> split 15


gridToText : List (List Cell) -> String
gridToText grid =
    grid
        |> gridToList
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
        |> listToGrid


radioBoxes : List ( Msg, Bool, String ) -> Html Msg
radioBoxes options =
    div []
        (options
            |> List.map
                (\( msg, checked, label ) ->
                    div [ onClick msg, classList [ ( "radio", True ), ( "checked", checked ) ] ] [ text label ]
                )
        )


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
        , radioBoxes
            [ ( ChangeSymmetry NoSymmetry, model.symmetry == NoSymmetry, "No symmetry" )
            , ( ChangeSymmetry Rotate180, model.symmetry == Rotate180, "Rotational symmetry 180º" )
            , ( ChangeSymmetry HorizontalSymmetry, model.symmetry == HorizontalSymmetry, "Horizontal Symmetry" )
            , ( ChangeSymmetry VerticalSymmetry, model.symmetry == VerticalSymmetry, "Vertical Symmetry" )
            ]
        , radioBoxes
            [ ( ChangeHighlightClueStarts False, model.highlightClueStarts == False, "No highlights" )
            , ( ChangeHighlightClueStarts True, model.highlightClueStarts == True, "Highlight clue starts" )
            ]
        , radioBoxes
            [ ( ChangeClueNumberIndication False, model.indicateClueNumbers == False, "Don't indicate clue numbers" )
            , ( ChangeClueNumberIndication True, model.indicateClueNumbers == True, "Indicate clue numbers" )
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
    box-sizing: border-box;
    -- font-size: 1.2em;
    position: relative;
}

.number {
    font-size: 0.8em;
    position: absolute;
    top: 2px;
    left: 2px;
}

.fill {
    display: flex;
    justify-content: center;
    align-items: center;
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



-- from https://stackoverflow.com/questions/31932683/transpose-in-elm-without-maybe


transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
            (x :: heads) :: transpose (xs :: tails)
