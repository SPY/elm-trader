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
import Component.ChartTabs as ChartTabs

type Event = Noop
    | Quotes Quotes.Message
    | SymbolsTable SymbolsTable.Event
    | HistoryLoaded String SymbolHistory.Period SymbolHistory.HistoryChunk
    | ChartTabs ChartTabs.Event

type alias State = {
    symbols : SymbolsTable.State,
    history : SymbolHistory.History,
    chartTabs : ChartTabs.State
}

init : (State, Effects Event)
init =
    let history = SymbolHistory.empty in
    let (chartsSt, chartEffs) = ChartTabs.init history in
    ({
        symbols = SymbolsTable.init,
        history = history,
        chartTabs = chartsSt
    }, Effects.map (always Noop) chartEffs)

update : Event -> State -> (State, Effects Event)
update event st = case event of
    Noop ->
        (st, Effects.none)
    Quotes msg ->
        ({ st | symbols <- SymbolsTable.update msg st.symbols }, Effects.none)
    SymbolsTable (SymbolsTable.OpenChart symbol) ->
        let (chartsSt, chartEffs) = ChartTabs.addChart symbol st.chartTabs in
        ({ st | chartTabs <- chartsSt }, Effects.map (always Noop) chartEffs)
    HistoryLoaded sym period chunk ->
        let history' = SymbolHistory.addChunk sym period chunk st.history in
        let (tabs, effs) = ChartTabs.update (ChartTabs.HistoryUpdate history') st.chartTabs in
        ({ st | history <- history', chartTabs <- tabs }, Effects.map (always Noop) effs)
    ChartTabs ev ->
        let (chartsSt, chartEffs) = ChartTabs.update ev st.chartTabs in
        ({ st | chartTabs <- chartsSt }, Effects.map (always Noop) chartEffs)

render : Address Event -> State -> Html
render addr st = div [class "app"] [
        Layout.render Layout.Horizontal [
            { size = Just 260, content = SymbolsTable.render (forwardTo addr SymbolsTable) st.symbols },
            { size = Nothing, content = ChartTabs.render (forwardTo addr ChartTabs) st.chartTabs }
        ]
    ]

historyToEvent : Result String (String, SymbolHistory.Period, SymbolHistory.HistoryChunk) -> Event
historyToEvent = toMaybe >> Maybe.map (\(s, p, h) -> HistoryLoaded s p h) >> withDefault Noop

inputs = [
        Signal.map Quotes Quotes.quotes,
        Signal.map (Maybe.map historyToEvent >> withDefault Noop) SymbolHistory.response
    ]