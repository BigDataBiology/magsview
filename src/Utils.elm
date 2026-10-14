module Utils exposing (mkTooltipQuestionMark, mkTooltip, showWithCommas, legendItem)

{-| Small helpers shared between pages: tooltips, number formatting and legends. -}

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


{-| Format an integer with thousands separators, e.g. 1234567 -> "1,234,567".
-}
showWithCommas : Int -> String
showWithCommas n =
    let
        addCommas s =
            if String.length s <= 3
                then s
            else (addCommas (String.slice 0 (String.length s - 3) s)) ++ "," ++ String.slice (String.length s - 3) (String.length s) s
    in
        addCommas (String.fromInt n)


{-| A colour swatch followed by its label, for use in legends.
-}
legendItem : String -> String -> Html msg
legendItem color label =
    Html.span
        [ HtmlAttr.style "display" "inline-flex"
        , HtmlAttr.style "align-items" "center"
        , HtmlAttr.style "gap" "0.3em"
        ]
        [ Html.span
            [ HtmlAttr.style "display" "inline-block"
            , HtmlAttr.style "width" "14px"
            , HtmlAttr.style "height" "14px"
            , HtmlAttr.style "background-color" color
            , HtmlAttr.style "border" "1px solid #999"
            ]
            []
        , Html.text label
        ]
