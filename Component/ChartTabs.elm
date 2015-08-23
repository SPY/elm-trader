module Component.ChartTabs (init, update, render, addChart, State, Event) where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Signal exposing (Address, forwardTo)

import SymbolHistory
import Component.Tabs as Tabs

type alias State = {
    charts: List String,
    period: SymbolHistory.Period,
    tabs: Tabs.State
}

type Event = OpenChart String | TabsEvent Tabs.Event

loadMoreHistory : String -> SymbolHistory.Period -> Effects ()
loadMoreHistory symbol period =
    let request = { symbol = symbol, year = 2015, period = period, from = 54722, to = 67298 } in
    Effects.task <| SymbolHistory.request request

init : (State, Effects ())
init = addChart "EURUSD.m" {
        charts = [],
        period = SymbolHistory.M15,
        tabs = Tabs.init
    }

update : Event -> State -> (State, Effects ())
update event st = case event of
    OpenChart symb -> addChart symb st
    TabsEvent ev -> ({ st | tabs <- Tabs.update ev st.tabs }, Effects.none)

addChart : String -> State -> (State, Effects ())
addChart chart st = ({ st |
        charts <- st.charts ++ [chart],
        tabs <- Tabs.activate chart st.tabs
    }, loadMoreHistory chart st.period)

render : Address Event -> State -> Html
render addr st =
    let chartTab chart = { title = chart, content = \() -> div [class "chart"] [text chart], id = chart } in
    let chartTabs = List.map chartTab st.charts in
    Tabs.render (forwardTo addr TabsEvent) st.tabs chartTabs