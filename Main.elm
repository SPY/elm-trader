import Signal exposing (Signal)
import Html exposing (Html)
import Task exposing (Task)

import StartApp
import Effects exposing (Never)

import App
import SymbolHistory

app = StartApp.start {
        init = App.init,
        update = App.update,
        view = App.render,
        inputs = App.inputs
    }

main : Signal Html
main = app.html

port history : Signal (Task x ())
port history = SymbolHistory.task

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks