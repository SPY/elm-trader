import Signal exposing (..)
import Task exposing (..)
import Html exposing (Html)

import StartApp
import Effects

import Quotes
import SymbolsTable
import App
import SymbolHistory

app = StartApp.start {
        init = (App.init, Effects.none),
        update = App.update,
        view = App.render,
        inputs = [Signal.map App.Quotes <| Quotes.quotes quotes]
    }

main : Signal Html
main = app.html

port quotes : Signal (String, Maybe String)