module SymbolHistory (load, log) where

import Native.SymbolHistory

import Task exposing (Task)

load : String -> Int -> String -> Int -> Int -> Task x String
load symbol year period from to =
    Native.SymbolHistory.load symbol year period from to

log : String -> Task x ()
log msg = 
    Native.SymbolHistory.log msg