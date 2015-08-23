module App (init, update, render, inputs, Event(..)) where

import Signal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Effects exposing (Effects)
import Maybe exposing (withDefault)
import Result exposing (toMaybe)

import Quotes
import SymbolsTable
import SymbolHistory

type Event = Noop
    | Quotes Quotes.Message
    | SymbolsTable SymbolsTable.Event
    | HistoryLoaded SymbolHistory.History

type alias State = {
    symbols : SymbolsTable.State,
    charts : List String,
    history : List SymbolHistory.History
}

init : State
init = {
        symbols = SymbolsTable.init,
        charts = [],
        history = []
    }

update : Event -> State -> (State, Effects Event)
update event st = case event of
    Noop ->
        (st, Effects.none)
    Quotes msg ->
        ({ st | symbols <- SymbolsTable.update msg st.symbols }, Effects.none)
    SymbolsTable (SymbolsTable.OpenChart symbol) ->
        let requestHistory = Effects.task (SymbolHistory.request symbol) |> Effects.map (always Noop) in
        ({ st | charts <-  st.charts ++ [symbol] }, requestHistory)
    HistoryLoaded history ->
        ({ st | history <-  st.history ++ [history] }, Effects.none)

render : Address Event -> State -> Html
render addr st = div [class "app"] [
        SymbolsTable.render (forwardTo addr SymbolsTable) st.symbols,
        div [class "charts"] <| List.map (\sym -> div [class "chart"] [text sym]) st.charts
    ]

historyToEvent : Result String SymbolHistory.History -> Event
historyToEvent = toMaybe >> Maybe.map HistoryLoaded >> withDefault Noop

inputs quotes = [
        Signal.map Quotes <| Quotes.quotes quotes,
        Signal.map (Maybe.map historyToEvent >> withDefault Noop) <| SymbolHistory.response
    ]