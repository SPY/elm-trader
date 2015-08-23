module Component.Layout (render, Direction(..)) where

import Html exposing (div, Html)
import Html.Attributes exposing (class, style, classList)

type Direction = Horizontal | Vertical

type alias Item = {
    size: Maybe Int,
    content: Html
}

renderItem : Direction -> Item -> Html
renderItem dir item =
    let attrs = case item.size of
        Just size ->
            let attr = if dir == Horizontal then "width" else "height" in
            [style [(attr, toString size ++ "px")], class "layout__item"]
        Nothing ->
            [class "layout__item layout__item_autosize_yes"]
    in
    div attrs [item.content]

render : Direction -> List Item -> Html
render dir =
    let cls = classList [
        ("layout", True),
        ("layout_direction_horizontal", dir == Horizontal),
        ("layout_direction_vertical", dir == Vertical)
    ]
    in div [cls] << List.map (renderItem dir)