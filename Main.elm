import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Snake = List (Coords)
type alias Apple = Coords
type alias Coords = { x:Int, y:Int }
type alias Screen = List (List (Color))

apple : Apple
apple = { x = 1, y = 2 }

snake : Snake
snake =
  [ { x = 3, y = 5 }
  , { x = 4, y = 5 }
  , { x = 5, y = 5 }
  , { x = 6, y = 5 }
  , { x = 6, y = 4 }
  , { x = 6, y = 3 }
  ]

main =
  emptyScreen 10
    |> drawApple apple
    |> drawSnake snake
    |> printScreen

emptyScreen : Int -> Screen
emptyScreen size =
  let
    row = repeat size white
  in
    repeat size row

printScreen : Screen -> Element
printScreen screen =
  let
    printRow row = map cell row
      |> flow right
  in
    map printRow screen
      |> flow down

drawApple : Apple -> Screen -> Screen
drawApple = colorCell red

drawSnake : Snake -> Screen -> Screen
drawSnake snake screen =
  foldr (colorCell green) screen snake

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


