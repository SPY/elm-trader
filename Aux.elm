module Aux (getCurrentTime, dimensions) where

import Time exposing (Time)
import Task exposing (Task)

import Native.Aux

getCurrentTime : Task x Time
getCurrentTime = Native.Aux.getCurrentTime

dimensions : Task x (Int, Int)
dimensions = Native.Aux.dimensions