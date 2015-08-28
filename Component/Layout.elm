module Component.Layout (init, update, render, Direction(..), State) where

import Html exposing (div, Html)
import Html.Attributes exposing (class, style, classList)

type Direction = Horizontal | Vertical

type alias Dimensions = { width: Int, height: Int }

type alias Item = {
    size: Maybe Int
}

type alias State = {
    dimensions: Dimensions,
    direction: Direction,
    items: List Item
}

init : Direction -> List (Maybe Int) -> State
init dir sizes =
    let items = List.map (\size -> { size = size }) sizes in
    { direction = dir, items = items, dimensions = { width = 0, height = 0 } }

add : Maybe Int -> State -> State
add size st = { st | items <- st.items ++ [{ size = size }] }

update : Dimensions -> State -> State
update dims st = { st | dimensions <- dims }

renderItem : Direction -> Dimensions -> Item -> (Dimensions -> Html) -> Html
renderItem dir dims item render =
    let attrs = case item.size of
        Just size ->
            let attr = if dir == Horizontal then "width" else "height" in
            [style [(attr, toString size ++ "px")], class "layout__item"]
        Nothing ->
            [class "layout__item layout__item_autosize_yes"]
    in
    div attrs [render dims]

render : State -> List (Dimensions -> Html) -> Html
render st renders =
    let cls = classList [
        ("layout", True),
        ("layout_direction_horizontal", st.direction == Horizontal),
        ("layout_direction_vertical", st.direction == Vertical)
    ]
    in div [cls] <| List.map2 (renderItem st.direction st.dimensions) st.items renders