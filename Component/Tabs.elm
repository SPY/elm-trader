module Component.Tabs (render, Event(..), State, update, init) where

import List exposing (head, filter)
import Maybe exposing (withDefault)
import Signal exposing (Address)

import Html exposing (div, Html, span, text)
import Html.Attributes exposing (class, style, classList)
import Html.Events exposing (onClick)

type Event = ActiveTab String

type alias Tab = {
    id: String,
    title: String,
    content: () -> Html
}

type alias State = {
    activeTab: String
}

emptyTab = {
        id = "empty",
        title = "Empty",
        content = \() -> span [] [text "No content"]
    }

getActive : String -> List Tab -> Tab
getActive id = filter (.id >> ((==) id)) >> head >> withDefault emptyTab

renderHead : Address Event -> String -> Tab -> Html
renderHead addr active tab =
    let cls = classList [("tabs__tab", True), ("tabs__tab_active_yes", active == tab.id)] in
    div [cls, onClick addr <| ActiveTab tab.id] [text tab.title]

render : Address Event -> State -> List Tab -> Html
render addr st items =
    let active = getActive st.activeTab items in
    let tabs = if List.length items > 0 then items else [emptyTab] in
    div [class "tabs"] [
        div [class "tabs__head"] <| List.map (renderHead addr active.id) tabs,
        div [class "tabs__content"] [active.content ()]
    ]

update : Event -> State -> State
update (ActiveTab id) st = { activeTab = id }

init : State
init = { activeTab = "empty" }