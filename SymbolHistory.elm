module SymbolHistory (load, log, Period(..), forSymbol, History, OHLC) where

import Native.SymbolHistory

import Task exposing (Task, andThen, fromResult)
import Array exposing (..)
import Json.Decode exposing (..)

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

load : String -> Int -> Period -> Int -> Int -> Task String History
load symbol year period from to =
    Native.SymbolHistory.load symbol year (toString period) from to `Task.andThen` \json ->
    decodeString (historyDecoder period) json |> fromResult


log : String -> Task x ()
log msg = 
    Native.SymbolHistory.log msg

historyDecoder : Period -> Decoder History
historyDecoder p =
    let makeHistory ms ohlcs = { period = p, median = ms, ohlc = ohlcs } in
    let medians = array <| maybe float in
    let makeOHLC o h l c = { open = o, high = h, low = l, close = c } in
    let ohlc = array <| maybe <| object4 makeOHLC ("s" := float) ("h" := float) ("l" := float) ("e" := float) in
    object2 makeHistory ("m" := medians) ("ohlc" := ohlc)

forSymbol : String -> Task String History
forSymbol symbol = load symbol 2015 M5 54722 67298