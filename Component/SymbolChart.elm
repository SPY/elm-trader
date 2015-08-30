module Component.SymbolChart (State, init, render, update, Action(..)) where

import Maybe
import List
import Array exposing (Array)

import Effects as Effects exposing (Effects)
import Html exposing (Html, div, text, fromElement)
import Html.Attributes exposing (class)
import Graphics.Collage as Canvas
import Color

import SymbolHistory
import Component.Layout as Layout

import Debug

type Action = UpdateData SymbolHistory.History

type alias State = {
    symbol: String,
    period: SymbolHistory.Period,
    data: Maybe (Array (Maybe SymbolHistory.OHLC))
}

init : String -> SymbolHistory.Period -> SymbolHistory.History -> (State, Effects ())
init sym period history =
    let getData = SymbolHistory.last sym period 200 history |> Effects.task in
    ({ symbol = sym, period = period, data = Nothing }, getData)

draw : Int -> Int -> Array SymbolHistory.OHLC -> Html
draw width height data =
    let (l, h) = extrema data in
    let toHeight val = toFloat height * (val / (h - l)) in
    let y val = toHeight (val - l) - (toFloat height/2) in
    let candle idx bar =
        let h = toHeight <| abs <| bar.open - bar.close in
        let shape = Canvas.rect (toFloat <| barWidth - 2) h in
        let barForm = if bar.open > bar.close
            then Canvas.filled Color.black shape
            else Canvas.outlined (Canvas.solid Color.black) shape
        in
        let top =
            let h' = toHeight <| bar.high - max bar.open bar.close in
            let path = Canvas.segment (0, h/2) (0, h/2 + h') in
            Canvas.traced (Canvas.solid Color.black) path
        in
        let bottom =
            let h' = toHeight <| min bar.open bar.close - bar.low in
            let path = Canvas.segment (0, negate h/2) (0, (negate h/2) - h') in
            Canvas.traced (Canvas.solid Color.black) path
        in
        let x = (negate <| toFloat width / 2) + (toFloat <| idx * barWidth) - (toFloat barWidth / 2) in
        Canvas.move (x, y <| (bar.open + bar.close) / 2) <| Canvas.group [barForm, top, bottom]
    in fromElement <| Canvas.collage width height <| Array.toList <| Array.indexedMap candle data

barWidth : Int
barWidth = 10

fromJust : Maybe a -> a
fromJust (Just val) = val

extrema : Array SymbolHistory.OHLC -> (Float, Float)
extrema data =
    let (Just first) = Array.get 0 data in
    Array.foldl (\bar (l, h)-> (min l bar.low, max h bar.high)) (first.low, first.high) data

clipData : Int -> Array (Maybe SymbolHistory.OHLC) -> Array SymbolHistory.OHLC
clipData width data =
    let desired = ceiling <| toFloat width / toFloat barWidth in
    let isJust = Maybe.map (always True) >> Maybe.withDefault False in
    let pure = Array.filter isJust >> Array.map fromJust <| data in
    let len = Array.length data in
    let bars = min len desired in
    Array.slice (len - bars) desired pure

render : State -> Layout.Dimensions -> Html
render st dims =
    let width = dims.width - 45 in -- hack
    let height = dims.height - 69 in -- hack
    let content = Maybe.map (clipData width >> draw width height) st.data |> Maybe.withDefault (text "Loading data...") in
    div [class "chart"] [content]

update : Action -> State -> (State, Effects ())
update action st = case action of
    UpdateData history -> case SymbolHistory.lookup st.symbol st.period history of
        Just data -> ({ st | data <- List.head data |> Maybe.map .ohlc }, Effects.none)
        Nothing -> (st, Effects.none)