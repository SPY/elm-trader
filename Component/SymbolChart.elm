module Component.SymbolChart (State, init, render) where

import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import SymbolHistory

type alias State = {
    symbol: String
}

loadMoreHistory : String -> SymbolHistory.Period -> Effects ()
loadMoreHistory symbol period =
    let request = { symbol = symbol, year = 2015, period = period, from = 54722, to = 67298 } in
    Effects.task <| SymbolHistory.request request

init : String -> (State, Effects ())
init sym = ({ symbol = sym }, Effects.none)

render : State -> Html
render st = div [class "chart"] [text st.symbol]