module Utils exposing (mkTooltipQuestionMark, mkTooltip)

{-| This module provides utility functions for creating tooltips in HTML elements. -}

import Html exposing (Html)
import Html.Attributes as HtmlAttr


mkTooltip : String -> Html msg -> Html msg
mkTooltip tooltipText htmlElement =
    Html.span
        [ HtmlAttr.class "mv-tooltip"
        , HtmlAttr.attribute "data-text" tooltipText
        ]
        [ htmlElement ]

mkTooltipQuestionMark : String -> Html msg
mkTooltipQuestionMark tooltipText =
    mkTooltip tooltipText
        (Html.span
                [ HtmlAttr.style "color" "#ccc"
                , HtmlAttr.style "background-color" "#000"
                , HtmlAttr.style "border-radius" "45%"
                , HtmlAttr.style "margin" "6px"
                , HtmlAttr.style "padding" "2px 4px"
                , HtmlAttr.style "font-size" "0.6em"
                , HtmlAttr.style "bottom" "8px"
                , HtmlAttr.style "position" "relative"
                ]
            [ Html.text "?" ])
