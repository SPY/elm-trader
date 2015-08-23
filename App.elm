module App (init, update, render, Event(..)) where

import Signal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)

import Quotes
import SymbolsTable

type Event = Quotes Quotes.Message | SymbolsTable SymbolsTable.Event

type alias State = { symbols : SymbolsTable.State, charts : List String }

init : State
init = {
        symbols = SymbolsTable.init,
        charts = []
    }

update : Event -> State -> State
update event st = case event of
    Quotes msg -> { st | symbols <- SymbolsTable.update msg st.symbols }
    SymbolsTable (SymbolsTable.OpenChart symbol) -> { st | charts <-  st.charts ++ [symbol] }

render : Address Event -> State -> Html
render addr st = div [class "app"] [
        SymbolsTable.render (forwardTo addr SymbolsTable) st.symbols,
        div [class "charts"] <| List.map (\sym -> span [class "chart"] [text sym]) st.charts
    ]