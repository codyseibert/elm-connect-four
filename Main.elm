module Main exposing (..)

import Html exposing (div, button, text)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import List
import Random
import Task
import Set
import Array
import Basics
import Debug
import Time exposing (..)


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
    | Tick Time
    | PiecePlaced
    | AIStep


type alias Board =
    List Cell


subscriptions model =
    every second Tick


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
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        cmd =
            Task.perform
                (\x ->
                    Tick 523942
                )
                (\a -> Tick a)
                (Time.now)
    in
        ( model, cmd )


type alias Model =
    { board : Board
    , isPlayersTurn : Bool
    , seed : Random.Seed
    , isGameOver : Bool
    , winner : State
    }


model =
    { board = createBoard 6 7
    , isPlayersTurn = True
    , seed = Random.initialSeed 31415
    , isGameOver = False
    , winner = Blue
    }


getRandomOpenColumn board seed =
    let
        openCells =
            board
                |> List.filter
                    (\c ->
                        c.state == Open
                    )

        gen =
            Random.int 0 (List.length openCells)

        openCellsArray =
            openCells |> Array.fromList

        ( ri, seed' ) =
            Random.step gen seed

        cell =
            Array.get ri openCellsArray
    in
        case cell of
            Nothing ->
                ( 0, seed' )

            Just a ->
                ( a.j, seed' )


updateCell cell board =
    board
        |> List.map
            (\c ->
                if c.i == cell.i && c.j == cell.j then
                    cell
                else
                    c
            )


dropCellIntoColumn j board color =
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
                updateCell { a | state = color } board


isGameOver board =
    let
        -- checkDiagnolUp i j color =
        --     let
        --         positions =
        --             [ 0, 1, 2, 3 ]
        --                 |> list.map
        --                     (\c ->
        --                         ( i + c, j + c )
        --                     )
        --     in
        --         False
        checkVertical col color =
            let
                rowsToCheck =
                    [ [ 0, 1, 2, 3 ]
                    , [ 1, 2, 3, 4 ]
                    , [ 2, 3, 4, 5 ]
                    ]

                rowsInCol =
                    board
                        |> List.filter
                            (\c ->
                                c.j == col && c.state == color
                            )
                        |> List.map
                            (\c ->
                                c.i
                            )

                isConnected =
                    rowsToCheck
                        |> List.map
                            (\s ->
                                rowsInCol
                                    |> Set.fromList
                                    |> Set.intersect (Set.fromList s)
                            )
                        |> List.any
                            (\set ->
                                4 == Set.size set
                            )
            in
                isConnected

        checkHorizontal row color =
            let
                colsToCheck =
                    [ [ 0, 1, 2, 3 ]
                    , [ 1, 2, 3, 4 ]
                    , [ 2, 3, 4, 5 ]
                    , [ 3, 4, 5, 6 ]
                    ]

                colsInRow =
                    board
                        |> List.filter
                            (\c ->
                                c.i == row && c.state == color
                            )
                        |> List.map
                            (\c ->
                                c.j
                            )

                isConnected =
                    colsToCheck
                        |> List.map
                            (\s ->
                                colsInRow
                                    |> Set.fromList
                                    |> Set.intersect (Set.fromList s)
                            )
                        |> List.any
                            (\set ->
                                4 == Set.size set
                            )
            in
                isConnected
    in
        let
            checkAllRows color =
                [0..5]
                    |> List.map
                        (\i ->
                            checkHorizontal i color
                        )
                    |> List.any (\a -> a)

            checkAllCols color =
                [0..6]
                    |> List.map
                        (\i ->
                            checkVertical i color
                        )
                    |> List.any (\a -> a)

            blueWon =
                checkAllRows Blue || checkAllCols Blue

            redWon =
                checkAllRows Red || checkAllCols Red
        in
            if blueWon then
                Blue
            else if redWon then
                Red
            else
                Open


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                t =
                    Basics.floor newTime

                seed' =
                    Random.initialSeed t
            in
                ( { model | seed = seed' }, Cmd.none )

        OnCellEnter cell ->
            if cell.state /= Open || model.isGameOver then
                ( model, Cmd.none )
            else
                ( { model | board = updateCell { cell | hovered = True } model.board }, Cmd.none )

        OnCellLeave cell ->
            ( { model | board = updateCell { cell | hovered = False } model.board }, Cmd.none )

        AIStep ->
            let
                ( randomCol, seed' ) =
                    getRandomOpenColumn model.board model.seed

                board' =
                    dropCellIntoColumn randomCol model.board Red
            in
                update PiecePlaced { model | seed = seed', board = board' }

        PiecePlaced ->
            let
                winner =
                    isGameOver model.board
            in
                if winner /= Open then
                    ( { model | isGameOver = True, winner = winner }, Cmd.none )
                else
                    ( model, Cmd.none )

        OnCellClick cell ->
            if cell.state /= Open || model.isGameOver then
                ( model, Cmd.none )
            else
                let
                    ( m, c ) =
                        update PiecePlaced { model | board = dropCellIntoColumn cell.j model.board Blue }
                in
                    if m.isGameOver then
                        ( m, Cmd.none )
                    else
                        update AIStep m


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
                 , ( "padding-top", "40px" )
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
                    else if cell.state == Red then
                        ( "background-color", "red" )
                    else
                        ( "", "" )
                   )
                 ]
                )
    in
        div []
            [ div [ style [ ( "color", "black" ) ] ]
                [ (if model.isGameOver == True then
                    "Game Over! " ++ (toString model.winner) ++ " has won!" |> text
                   else
                    text ""
                  )
                ]
            , div
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
