import List exposing (..)
import Color exposing (..)
import Graphics.Element exposing (Element)
import Screen
import Keyboard
import Signal
import Time exposing (fps)

type Direction = Up | Down | Right | Left

type alias Snake = List Screen.Coords
type alias Apple = Screen.Coords


boardSize : Int
boardSize = 10


apple : Apple
apple = { x = 1, y = 2 }


main : Signal Element
main =
  let
    snakeStream =
      Signal.map (\coords -> [coords]) position
  in
    Signal.map (drawScreen apple) snakeStream


drawScreen : Apple -> Snake -> Element
drawScreen apple snake =
  Screen.size boardSize
    |> drawApple apple
    |> drawSnake snake
    |> Screen.print


drawApple : Apple -> Screen.Screen -> Screen.Screen
drawApple = Screen.colorCell red


drawSnake : Snake -> Screen.Screen -> Screen.Screen
drawSnake snake screen =
  foldr (Screen.colorCell green) screen snake


position : Signal Screen.Coords
position =
  let
    startCoords = { x = 2, y = 3 }
    step : Direction -> Screen.Coords -> Screen.Coords
    step direction coords =
      case direction of
        Up ->
          { coords | y = (coords.y - 1) % boardSize }
        Down ->
          { coords | y = (coords.y + 1) % boardSize }
        Left ->
          { coords | x = (coords.x - 1) % boardSize }
        Right ->
          { coords | x = (coords.x + 1) % boardSize }
  in
    Signal.sampleOn (fps 2) direction
      |> Signal.foldp step startCoords


direction : Signal Direction
direction =
  let
    directionFromTurns : Int -> Direction
    directionFromTurns turns =
      case (turns % 4) of
        0 -> Right
        1 -> Down
        2 -> Left
        3 -> Up
        -- Fallback case to keep the compiler happy.
        _ -> Right
  in
    Keyboard.arrows
      |> Signal.map .x
      |> Signal.dropRepeats
      |> Signal.foldp (+) 0
      |> Signal.map directionFromTurns

