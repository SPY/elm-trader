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

requestBox : Signal.Mailbox (Maybe String)
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
    let request symbol = case symbol of
        Just sym -> (forSymbol sym |> Task.toResult) `Task.andThen` (Just >> Signal.send responseBox.address)
        Nothing -> Task.succeed ()
    in Signal.map request requestBox.signal

request : String -> Task x ()
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

forSymbol : String -> Task String History
forSymbol symbol = load symbol 2015 M5 54722 67298