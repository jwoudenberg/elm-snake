#!/bin/sh
elm-make app/Main.elm --output dist/index.html
surge ./dist --domain snake.jasperwoudenberg.com
