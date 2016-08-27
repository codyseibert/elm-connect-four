module Main exposing (..)

import Html exposing (div, button, text)
import Html.Attributes exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import List
import Random
import Set
import Array
import Debug


type State
    = Open
    | Blue
    | Red


type alias Cell =
    { i : Int
    , j : Int
    , hovered : Bool
    , state : State
    }


type Msg
    = OnCellEnter Cell
    | OnCellLeave Cell
    | OnCellClick Cell


type alias Board =
    List Cell


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


createBoard : Int -> Int -> Board
createBoard rows cols =
    cartesian [0..rows - 1] [0..cols - 1]
        |> List.map
            (\( i, j ) ->
                { i = i
                , j = j
                , hovered = False
                , state = Open
                }
            )


main =
    beginnerProgram { model = model, view = view, update = update }


model =
    { board = createBoard 6 7
    }


updateCell cell board =
    board
        |> List.map
            (\c ->
                if c.i == cell.i && c.j == cell.j then
                    cell
                else
                    c
            )


dropCellIntoColumn j board =
    let
        rows =
            [0..5] |> List.reverse

        openCellsInRow =
            board
                |> List.filter
                    (\c ->
                        c.j == j && c.state == Open
                    )

        openCellInRow i =
            openCellsInRow
                |> List.filter
                    (\c ->
                        c.i == i
                    )

        cellToFill =
            rows |> List.map openCellInRow |> List.concat |> List.head
    in
        case cellToFill of
            Nothing ->
                board

            Just a ->
                updateCell { a | state = Blue } board


update msg model =
    case msg of
        OnCellEnter cell ->
            { model | board = updateCell { cell | hovered = True } model.board }

        OnCellLeave cell ->
            { model | board = updateCell { cell | hovered = False } model.board }

        OnCellClick cell ->
            { model | board = dropCellIntoColumn cell.j model.board }


view model =
    let
        cellSize =
            100

        getX j w =
            j * w + 5

        getY i w =
            i * w + 5

        boardStyle =
            style
                ([ ( "background-color", "yellow" )
                 , ( "width", "700px" )
                 , ( "height", "600px" )
                 , ( "margin", "0 auto" )
                 , ( "margin-top", "40px" )
                 , ( "position", "relative" )
                 ]
                )

        cellStyle cell =
            style
                ([ ( "position", "absolute" )
                 , ( "top", (toString (getY cell.i cellSize)) ++ "px" )
                 , ( "left", (toString (getX cell.j cellSize)) ++ "px" )
                 , ( "background-color", "white" )
                 , ( "width", "90px" )
                 , ( "height", "90px" )
                 , ( "border-radius", "50%" )
                 , (if cell.hovered then
                        ( "border", "2px solid red" )
                    else
                        ( "", "" )
                   )
                 , (if cell.state == Blue then
                        ( "background-color", "blue" )
                    else
                        ( "", "" )
                   )
                 ]
                )
    in
        div []
            [ div
                [ boardStyle ]
                (model.board
                    |> List.map
                        (\c ->
                            div
                                [ cellStyle c
                                , onMouseEnter (OnCellEnter c)
                                , onMouseLeave (OnCellLeave c)
                                , onClick (OnCellClick c)
                                ]
                                []
                        )
                )
            ]
