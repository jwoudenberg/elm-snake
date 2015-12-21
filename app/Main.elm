import List exposing (..)
import Color exposing (..)
import Graphics.Element exposing (Element)
import Screen
import Keyboard
import Signal
import Time exposing (fps)

type Direction = Up | Down | Right | Left

type alias Snake = Signal (List Screen.Coords)
type alias Apple = Signal Screen.Coords


main : Signal Element
main =
  drawScreen apple snake


boardSize : Int
boardSize = 10


apple : Apple
apple =
  Signal.constant { x = 1, y = 2 }


snake : Snake
snake =
  Signal.map (\coords -> [coords]) position


drawScreen : Apple -> Snake -> Signal Element
drawScreen apple snake =
  let
    drawApplePos =
      Screen.colorCell red
    drawSnakePos snakePos screen =
      foldr (Screen.colorCell green) screen snakePos
    drawFrame applePos snakePos =
      Screen.size boardSize
        |> drawApplePos applePos
        |> drawSnakePos snakePos
        |> Screen.print
  in
    Signal.map2 drawFrame apple snake


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

