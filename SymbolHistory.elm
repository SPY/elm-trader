module SymbolHistory (Period(..), History, HistoryChunk, OHLC, task, request, response, addChunk, empty) where

import Native.SymbolHistory

import Task exposing (Task, andThen, fromResult)
import Array exposing (..)
import Json.Decode exposing (..)
import Signal
import Maybe
import Dict exposing (Dict)

type Period = M1 | M5 | M15

toInt : Period -> Int
toInt p = case p of
    M1 -> 1
    M5 -> 2
    M15 -> 3

type alias OHLC = {
    open: Float,
    high: Float,
    low: Float,
    close: Float
}

type alias HistoryChunk = {
    median: Array (Maybe Float),
    ohlc: Array (Maybe OHLC)
}

type alias Request = {
    symbol: String,
    period: Period,
    year: Int,
    from: Int,
    to: Int
}

type alias History = Dict (String, Int) (List HistoryChunk)

empty : History
empty = Dict.empty

requestBox : Signal.Mailbox (Maybe Request)
requestBox = Signal.mailbox Nothing

responseBox : Signal.Mailbox (Maybe (Result String (String, Period, HistoryChunk)))
responseBox = Signal.mailbox Nothing

load : String -> Int -> Period -> Int -> Int -> Task String HistoryChunk
load symbol year period from to =
    Native.SymbolHistory.load symbol year (toString period) from to `Task.andThen` \json ->
    decodeString historyDecoder json |> fromResult

log : String -> Task x ()
log msg = 
    Native.SymbolHistory.log msg

task : Signal (Task x ())
task =
    let request req = case req of
        Just r ->
            let t = load r.symbol r.year r.period r.from r.to in
            (t |> Task.toResult) `Task.andThen` (Result.map (\h -> (r.symbol, r.period, h)) >> Just >> Signal.send responseBox.address)
        Nothing -> Task.succeed ()
    in Signal.map request requestBox.signal

request : Request -> Task x ()
request = Just >> Signal.send requestBox.address

response : Signal (Maybe (Result String (String, Period, HistoryChunk)))
response = responseBox.signal

historyDecoder : Decoder HistoryChunk
historyDecoder =
    let makeHistory ms ohlcs = { median = ms, ohlc = ohlcs } in
    let medians = array <| maybe float in
    let makeOHLC o h l c = { open = o, high = h, low = l, close = c } in
    let ohlc = array <| maybe <| object4 makeOHLC ("s" := float) ("h" := float) ("l" := float) ("e" := float) in
    object2 makeHistory ("m" := medians) ("ohlc" := ohlc)

addChunk : String -> Period -> HistoryChunk -> History -> History
addChunk sym period chunk =
    let add = Maybe.withDefault [] >> ((::) chunk) >> Just in
    Dict.update (sym, toInt period) add

last : String -> Period -> Int -> History -> Task x ()
last sym period num history = Task.succeed ()