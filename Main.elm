import Signal exposing (..)
import Task exposing (..)
import Html exposing (Html)

import Quotes
import SymbolsTable
import App
import SymbolHistory

main : Signal Html
main =
    let events = mailbox Nothing in
    let qoutes = Signal.map (App.Quotes >> Just) <| Quotes.quotes quotes in
    let state = Signal.foldp (\(Just event) -> App.update event) App.init <| merge events.signal qoutes in
    Signal.map (App.render <| forwardTo events.address Just) state

port history : Signal (Task x ())
port history = constant <| (SymbolHistory.load "AUDUSD.m" 2015 "M5" 54722 67298 `andThen` SymbolHistory.log)

port quotes : Signal (String, Maybe String)