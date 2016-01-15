module Screen (..) where

import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text


type alias Coords =
    { x : Int, y : Int }


type alias Screen =
    List (List Color)


size : Int -> Screen
size screenSize =
    let
        row = repeat screenSize white
    in
        repeat screenSize row


printGame : Screen -> Element
printGame screen =
    let
        printRow row =
            map cell row
                |> flow right
    in
        map printRow screen
            |> flow down


printEnd : Element
printEnd =
    Text.fromString "GAME OVER!"
        |> Graphics.Element.centered


printScore : Int -> Element
printScore score =
    toString score
        |> (++) "SCORE: "
        |> Text.fromString
        |> centered


printPage : ( Int, Int ) -> Element -> Element -> Element
printPage ( width, height ) score game =
    let
        centerScreen =
            container width height middle

        frameGame =
            container width 100 middle

        frameScore =
            container width 40 middle

        frameFooter =
            container width 40 middle
    in
        flow down [ frameScore score, frameGame game, frameFooter footer ]
            |> centerScreen


footer : Element
footer =
    let
        url = "github.com/jwoudenberg/elm-snake"
    in
        Text.link ("http://" ++ url) (Text.fromString url)
            |> centered


colorCell : Color -> Coords -> Screen -> Screen
colorCell color { x, y } screen =
    let
        updateRow = update x (always color)
    in
        update y updateRow screen


cell : Color -> Element
cell color =
    let
        cellSize = 10

        cellShape = square (toFloat cellSize)

        cellOutline = outlined (solid black) cellShape

        cellFill = filled color cellShape
    in
        collage
            cellSize
            cellSize
            [ cellFill
            , cellOutline
            ]


update : Int -> (a -> a) -> List a -> List a
update index updater list =
    case ( index, list ) of
        ( _, [] ) ->
            []

        ( 0, value :: tail ) ->
            updater value :: tail

        ( _, head :: tail ) ->
            head :: (update (index - 1) updater tail)
