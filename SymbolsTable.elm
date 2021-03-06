module SymbolsTable (render, update, init, State, Event(..)) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Signal exposing (..)
import Dict exposing (..)
import Maybe
import String exposing (..)

import Quotes

type Trend = Up | Down | Neutral

type alias Symbol = {
    digits: Int,
    ask: { trend: Trend, val: Float },
    bid: { trend: Trend, val: Float }
}

type alias State = Dict String Symbol

type Event = OpenChart String

symbolRow : Address Event -> (String, Symbol) -> Html
symbolRow addr (name, symbol) = 
    let classWithTrend trend cls = case trend of
        Up -> class <| cls ++ " symbols-table_trend_up"
        Down -> class <| cls ++ " symbols-table_trend_down"
        Neutral -> class cls
    in
    tr [class "symbols-table__row", onClick addr <| OpenChart name] [
        td [class "symbols-table__name"] [text name],
        td [classWithTrend symbol.ask.trend "symbols-table__ask"] [text <| toFixed symbol.digits symbol.ask.val],
        td [classWithTrend symbol.bid.trend "symbols-table__bid"] [text <| toFixed symbol.digits symbol.bid.val]
    ]

render : Address Event -> State -> Html
render addr symbols =
    div [class "symbols-table"] [
        table [class "symbols-table__table"] ([
            tr [class "symbols-table__head"] [
                th [class "symbols-table__name"] [text "Symbol"],
                th [class "symbols-table__ask"] [text "Ask"],
                th [class "symbols-table__bid"] [text "Bid"]
            ]
        ] ++ (List.map (symbolRow addr) <| Dict.toList symbols))
    ]

update : Quotes.Message -> State -> State
update msg syms =
    let updateVal old new = if old.val == 0 then { old | val <- new } else case compare old.val new of
        LT -> { val = new, trend = Up }
        EQ -> { old | val <- new }
        GT -> { val = new, trend = Down }
    in
    let updateQuotes (sym, bid, ask) syms =
        Dict.update sym (Maybe.map (\s -> { s | bid <- updateVal s.bid bid, ask <- updateVal s.ask ask })) syms
    in
    let insertSym (sym, digits) syms =
        Dict.insert sym {
                digits = digits,
                ask = { val = 0, trend = Neutral },
                bid = { val = 0, trend = Neutral }
            } syms
    in
    case msg of
        Quotes.Symbols sym qs -> List.foldl insertSym syms sym |> \syms -> List.foldl updateQuotes syms qs
        Quotes.Quotes qs -> List.foldl updateQuotes syms qs

init : State
init = Dict.empty

toFixed : Int -> Float -> String
toFixed digits num = case split "." <| toString num of
    [one] -> one ++ "." ++ (repeat digits "0")
    [int, frac] ->
        let frac' = left digits frac in
        int ++ "." ++ frac' ++ (repeat (digits - length frac') "0")