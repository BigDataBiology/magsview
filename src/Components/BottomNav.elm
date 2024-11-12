module Components.BottomNav exposing (..)
import Html exposing (..)
import Html.Attributes as Hattr exposing (..)

bottomNav : Html msg
bottomNav =
    div [Hattr.class "bottomNav"] [
        a [] [text "MagsView"]
    ]