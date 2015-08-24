module Component.ChartTabs (init, update, render, addChart, State, Event) where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Signal exposing (Address, forwardTo)

import SymbolHistory
import Component.Tabs as Tabs
import Component.SymbolChart as Chart

type alias ChartTab = {
    symbol: String,
    id: String,
    chart: Chart.State
}

type alias State = {
    charts: List ChartTab,
    period: SymbolHistory.Period,
    tabs: Tabs.State,
    id: Int
}

type Event = OpenChart String | TabsEvent Tabs.Event

init : (State, Effects ())
init = addChart "EURUSD.m" {
        charts = [],
        period = SymbolHistory.M15,
        tabs = Tabs.init,
        id = 1
    }

update : Event -> State -> (State, Effects ())
update event st = case event of
    OpenChart symb -> addChart symb st
    TabsEvent ev -> ({ st | tabs <- Tabs.update ev st.tabs }, Effects.none)

addChart : String -> State -> (State, Effects ())
addChart symbol st = 
    let (chart, chartEffs) = Chart.init symbol in
    let tab = { symbol = symbol, chart = chart, id = symbol ++ (toString st.id) } in
    ({ st |
        charts <- st.charts ++ [tab],
        tabs <- Tabs.activate tab.id st.tabs,
        id <- st.id + 1
    }, chartEffs)

render : Address Event -> State -> Html
render addr st =
    let chartTab tab = { title = tab.symbol, content = \() -> Chart.render tab.chart, id = tab.id } in
    let chartTabs = List.map chartTab st.charts in
    Tabs.render (forwardTo addr TabsEvent) st.tabs chartTabs