import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Screen


type alias Snake = List Screen.Coords
type alias Apple = Screen.Coords


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


main : Element
main =
  Screen.size 10
    |> drawApple apple
    |> drawSnake snake
    |> Screen.print


drawApple : Apple -> Screen.Screen -> Screen.Screen
drawApple = Screen.colorCell red


drawSnake : Snake -> Screen.Screen -> Screen.Screen
drawSnake snake screen =
  foldr (Screen.colorCell green) screen snake
