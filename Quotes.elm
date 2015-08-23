module Quotes (quotes, Message(..)) where

import Signal
import Maybe exposing (andThen)
import Result
import Json.Decode exposing (..)

import Native.Quotes

type alias SymbolSettings = (String, Int)

type alias Quote = (String, Float, Float)

type Message = Symbols (List SymbolSettings) (List Quote) | Quotes (List Quote)

type ConnectionEvent = NotConnected | Open | Close | Message (Maybe Message)

messageDecoder : Decoder Message
messageDecoder = 
    let qoutes = ("md" := (list <| tuple3 (,,) string float float)) in
    let initQuotes = ("md0" := (list <| tuple4 (\a b c d -> (a, b, c)) string float float float)) in
    let symbols = ("sym" := (list <| tuple2 (,) string int)) in
    oneOf [
        object2 Symbols symbols initQuotes,
        object1 Quotes qoutes
    ]

toConnectionEvent : (String, String) -> ConnectionEvent
toConnectionEvent (msgType, payload) = case msgType of
    "notconnected" -> NotConnected
    "open" -> Open
    "close" -> Close
    "message" -> Message (parseMessage payload)

parseMessage : String -> Maybe Message
parseMessage = Result.toMaybe << decodeString messageDecoder

connection : Signal ConnectionEvent
connection = Signal.map toConnectionEvent Native.Quotes.quotes

quotes : Signal Message
quotes = Signal.filterMap onlyMessage (Quotes []) connection

onlyMessage : ConnectionEvent -> Maybe Message
onlyMessage event = case event of
    Message msg -> msg
    _ -> Nothing
