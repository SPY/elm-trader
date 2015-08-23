module SymbolHistory (Period(..), History, OHLC, task, request, response) where

import Native.SymbolHistory

import Task exposing (Task, andThen, fromResult)
import Array exposing (..)
import Json.Decode exposing (..)
import Signal
import Maybe

type Period = M1 | M5 | M15

type alias OHLC = {
    open: Float,
    high: Float,
    low: Float,
    close: Float
}

type alias History = {
    period: Period,
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

requestBox : Signal.Mailbox (Maybe Request)
requestBox = Signal.mailbox Nothing

responseBox : Signal.Mailbox (Maybe (Result String History))
responseBox = Signal.mailbox Nothing

load : String -> Int -> Period -> Int -> Int -> Task String History
load symbol year period from to =
    Native.SymbolHistory.load symbol year (toString period) from to `Task.andThen` \json ->
    decodeString (historyDecoder period) json |> fromResult

log : String -> Task x ()
log msg = 
    Native.SymbolHistory.log msg

task : Signal (Task x ())
task =
    let request req = case req of
        Just r ->
            let t = load r.symbol r.year r.period r.from r.to in
            (t |> Task.toResult) `Task.andThen` (Just >> Signal.send responseBox.address)
        Nothing -> Task.succeed ()
    in Signal.map request requestBox.signal

request : Request -> Task x ()
request = Just >> Signal.send requestBox.address

response : Signal (Maybe (Result String History))
response = responseBox.signal

historyDecoder : Period -> Decoder History
historyDecoder p =
    let makeHistory ms ohlcs = { period = p, median = ms, ohlc = ohlcs } in
    let medians = array <| maybe float in
    let makeOHLC o h l c = { open = o, high = h, low = l, close = c } in
    let ohlc = array <| maybe <| object4 makeOHLC ("s" := float) ("h" := float) ("l" := float) ("e" := float) in
    object2 makeHistory ("m" := medians) ("ohlc" := ohlc)