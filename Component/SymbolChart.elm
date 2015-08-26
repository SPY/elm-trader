module Component.SymbolChart (State, init, render, update, Action(..)) where

import Maybe
import List
import Array exposing (Array)

import Effects as Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import SymbolHistory

type Action = UpdateData SymbolHistory.History

type alias State = {
    symbol: String,
    period: SymbolHistory.Period,
    data: Maybe (Array (Maybe SymbolHistory.OHLC))
}

init : String -> SymbolHistory.Period -> SymbolHistory.History -> (State, Effects ())
init sym period history =
    let getData = SymbolHistory.last sym period 100 history |> Effects.task in
    ({ symbol = sym, period = period, data = Nothing }, getData)

render : State -> Html
render st =
    let content = Maybe.map (Array.length >> toString >> ((++) "Bars loaded: ")) st.data |> Maybe.withDefault "Loading data..." in
    div [class "chart"] [text content]

update : Action -> State -> (State, Effects ())
update action st = case action of
    UpdateData history -> case SymbolHistory.lookup st.symbol st.period history of
        Just data -> ({ st | data <- List.head data |> Maybe.map .ohlc }, Effects.none)
        Nothing -> (st, Effects.none)