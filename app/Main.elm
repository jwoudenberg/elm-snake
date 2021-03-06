module Main (..) where

import List exposing (..)
import Color exposing (..)
import Graphics.Element exposing (Element)
import Window
import Screen
import Keyboard
import Signal
import Random
import Time


type Direction
    = Up
    | Down
    | Right
    | Left


type alias Snake =
    Signal (List Screen.Coords)


type alias Apple =
    Signal Screen.Coords


main : Signal Element
main =
    drawPage


boardSize : Int
boardSize =
    10


drawPage : Signal Element
drawPage =
    let
        pageFrame frame score dimensions =
            Screen.printPage dimensions (Screen.printScore score) frame
    in
        Signal.map3 pageFrame screen score Window.dimensions


screen : Signal Element
screen =
    let
        drawApplePos =
            Screen.colorCell red

        drawSnakePos snakePos screen =
            foldr (Screen.colorCell green) screen snakePos

        drawFrame applePos snakePos =
            Screen.size boardSize
                |> drawApplePos applePos
                |> drawSnakePos snakePos
                |> Screen.printGame

        drawGame gameEnded applePos snakePos =
            if gameEnded then
                Screen.printEnd
            else
                drawFrame applePos snakePos
    in
        Signal.map3 drawGame gameOver apple snake


gameOver : Signal Bool
gameOver =
    Signal.foldp (||) False (snakeCollides snake)


score : Signal Int
score =
    let
        snakeLength = Signal.map length snake

        newScore ( newLength, gameEnded ) oldLength =
            if gameEnded then
                oldLength
            else
                newLength
    in
        Signal.foldp newScore 1 (Signal.map2 (,) snakeLength gameOver)


snake : Snake
snake =
    Signal.map (\( snakePos, _, _ ) -> snakePos) snakeAndApple


apple : Apple
apple =
    Signal.map (\( _, applePos, _ ) -> applePos) snakeAndApple


snakeCollides : Snake -> Signal Bool
snakeCollides snake =
    let
        snakePosIntersects snakePos =
            case snakePos of
                [] ->
                    False

                snakeHead :: snakeTail ->
                    any ((==) snakeHead) snakeTail
    in
        Signal.map snakePosIntersects snake


coordsGenerator : Random.Generator { x : Int, y : Int }
coordsGenerator =
    let
        maxCoord = boardSize - 1

        pairGenerator =
            Random.pair (Random.int 0 maxCoord) (Random.int 0 maxCoord)

        pairToCoords ( x, y ) =
            { x = x, y = y }
    in
        Random.map pairToCoords pairGenerator


snakeAndApple : Signal ( List Screen.Coords, Screen.Coords, Random.Seed )
snakeAndApple =
    let
        startSnake =
            [ startCoords ]

        ( startApple, startSeed ) =
            Random.generate coordsGenerator (Random.initialSeed 42)

        nextTick newPos ( snakePos, applePos, seed ) =
            let
                appleEaten = applePos `List.member` snakePos

                snakeLength = List.length snakePos

                newSnakePos = newPos :: snakePos

                ( newApplePos, newSeed ) = Random.generate coordsGenerator seed
            in
                if appleEaten then
                    ( newSnakePos, newApplePos, newSeed )
                else
                    ( List.take snakeLength newSnakePos, applePos, seed )
    in
        Signal.foldp nextTick ( startSnake, startApple, startSeed ) position


startCoords : Screen.Coords
startCoords =
    { x = 2, y = 3 }


position : Signal Screen.Coords
position =
    let
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
        Signal.sampleOn (Time.fps 2) direction
            |> Signal.foldp step startCoords


direction : Signal Direction
direction =
    let
        directionFromTurns : Int -> Direction
        directionFromTurns turns =
            case (turns % 4) of
                0 ->
                    Right

                1 ->
                    Down

                2 ->
                    Left

                3 ->
                    Up

                -- Fallback case to keep the compiler happy.
                _ ->
                    Right
    in
        Keyboard.arrows
            |> Signal.map .x
            |> Signal.dropRepeats
            |> Signal.foldp (+) 0
            |> Signal.map directionFromTurns
