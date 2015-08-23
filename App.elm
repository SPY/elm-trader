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

import Component.Layout as Layout
import Component.Tabs as Tabs

type Event = Noop
    | Quotes Quotes.Message
    | SymbolsTable SymbolsTable.Event
    | HistoryLoaded SymbolHistory.History
    | ChartTabs Tabs.Event

type alias State = {
    symbols : SymbolsTable.State,
    charts : List String,
    history : List SymbolHistory.History,
    chartTabs : Tabs.State
}

init : (State, Effects Event)
init = 
    let request = { symbol = "EURUSD.m", year = 2015, period = SymbolHistory.M5, from = 54722, to = 67298 } in
    let requestHistory = Effects.task (SymbolHistory.request request) |> Effects.map (always Noop) in
    ({
        symbols = SymbolsTable.init,
        charts = ["EURUSD.m"],
        history = [],
        chartTabs = Tabs.update (Tabs.ActiveTab "EURUSD.m") Tabs.init
    }, requestHistory)

update : Event -> State -> (State, Effects Event)
update event st = case event of
    Noop ->
        (st, Effects.none)
    Quotes msg ->
        ({ st | symbols <- SymbolsTable.update msg st.symbols }, Effects.none)
    SymbolsTable (SymbolsTable.OpenChart symbol) ->
        let request = { symbol = symbol, year = 2015, period = SymbolHistory.M5, from = 54722, to = 67298 } in
        let requestHistory = Effects.task (SymbolHistory.request request) |> Effects.map (always Noop) in
        ({ st | charts <-  st.charts ++ [symbol], chartTabs <- Tabs.update (Tabs.ActiveTab symbol) st.chartTabs }, requestHistory)
    HistoryLoaded history ->
        ({ st | history <-  st.history ++ [history] }, Effects.none)
    ChartTabs ev ->
        ({ st | chartTabs <- Tabs.update ev st.chartTabs }, Effects.none)

render : Address Event -> State -> Html
render addr st = div [class "app"] [
        Layout.render Layout.Horizontal [
            { size = Just 260, content = SymbolsTable.render (forwardTo addr SymbolsTable) st.symbols },
            { size = Nothing, content = renderCharts addr st}
        ]
    ]

renderCharts : Address Event -> State -> Html
renderCharts addr st =
    let chartTab chart = { title = chart, content = \() -> div [class "chart"] [text chart], id = chart } in
    let chartTabs = List.map chartTab st.charts in
    Tabs.render (forwardTo addr ChartTabs) st.chartTabs chartTabs

historyToEvent : Result String SymbolHistory.History -> Event
historyToEvent = toMaybe >> Maybe.map HistoryLoaded >> withDefault Noop

inputs = [
        Signal.map Quotes Quotes.quotes,
        Signal.map (Maybe.map historyToEvent >> withDefault Noop) <| SymbolHistory.response
    ]