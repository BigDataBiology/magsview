module Components.Navbar exposing (..)
import Html exposing (..)
import Html.Attributes as Hattr exposing (..)

navbar : Html msg
navbar =
    div [Hattr.class "navbar"] [
        h2 [Hattr.class "navbar_title"] [text "MagsView"],
        div [Hattr.class "navbar_links"] [
            a [Hattr.href "/"] [text "Home"],
            a [Hattr.href "#"] [text "Genomes"],
            a [Hattr.href "#"] [text "Taxonomy"]
        ]
    ]