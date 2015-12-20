module Screen where

import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Coords = { x:Int, y:Int }
type alias Screen = List (List Color)


size : Int -> Screen
size screenSize =
  let
    row = repeat screenSize white
  in
    repeat screenSize row


print : Screen -> Element
print screen =
  let
    printRow row = map cell row
      |> flow right
  in
    map printRow screen
      |> flow down


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
    collage cellSize cellSize
      [ cellFill,
        cellOutline
      ]


update : Int -> (a -> a) -> List (a) -> List (a)
update index updater list =
  case (index, list) of
    (_, []) ->
      []
    (0, value :: tail) ->
      updater value :: tail
    (_, head :: tail) ->
      head :: (update (index - 1)  updater tail)
