module Component.SymbolChart (State, init, render, update, Action(..)) where

import Maybe
import List
import Array exposing (Array)

import Effects as Effects exposing (Effects)
import Html exposing (Html, div, text, fromElement)
import Html.Attributes exposing (class)
import Graphics.Collage as Canvas exposing (rect, move, group, traced, segment, solid, dashed)
import Color exposing (black, white, gray)

import SymbolHistory exposing (OHLC, Period, History)
import Component.Layout as Layout

import Debug

type Action = UpdateData SymbolHistory.History

type alias State = {
    symbol: String,
    period: SymbolHistory.Period,
    data: Maybe (Array (Maybe SymbolHistory.OHLC))
}

init : String -> Period -> History -> (State, Effects ())
init sym period history =
    let getData = SymbolHistory.last sym period 200 history |> Effects.task in
    ({ symbol = sym, period = period, data = Nothing }, getData)

type alias View = {
    y: Float -> Float,
    x: Float -> Float
}

drawCandle : View -> Int -> OHLC -> Canvas.Form
drawCandle view idx bar =
    let h = abs <| view.y bar.open - view.y bar.close in
    let shape = rect (toFloat <| barWidth - 2) h in
    let solidBlack = solid black in
    let body = if bar.open > bar.close
        then Canvas.filled black shape
        else Canvas.outlined solidBlack shape
    in
    let top =
        let h' = view.y bar.high - (view.y <| max bar.open bar.close) in
        traced solidBlack <| segment (0, h/2) (0, h/2 + h')
    in
    let bottom =
        let half = negate h/2 in
        let h' = (view.y <| min bar.open bar.close) - view.y bar.low in
        traced solidBlack <| segment (0, half) (0, half - h')
    in
    let x = view.x <|(toFloat <| idx * barWidth) - (toFloat barWidth / 2) in
    move (x, view.y <| (bar.open + bar.close) / 2) <| group [top, body, bottom]

drawGrid : Int -> Int -> (Float, Float) -> Canvas.Form
drawGrid width height (low, high) =
    let h = toFloat height - 2 in
    let y idx = (negate <| h/2) + h/9*(toFloat idx) in
    let level idx =
        let half = toFloat width/2 in
        traced (dashed gray) <| segment (negate half, y idx) (half, y idx)
    in group <| List.map level [0..9]

draw : Int -> Int -> Array SymbolHistory.OHLC -> Html
draw width height data =
    let (l, h) = extrema data in
    let y val = toFloat height * ((val - l) / (h - l)) - (toFloat height/2) in
    let x val = (negate <| toFloat width / 2) + val in
    let view = { y = y, x = x } in
    let candle = drawCandle view in
    let bars = Array.toList <| Array.indexedMap candle data in
    let grid = drawGrid width height (l, h) in
    fromElement <| Canvas.collage width height <| grid :: bars

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
    let len = Array.length pure in
    let bars = min len desired in
    Array.slice (len - bars) len pure

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