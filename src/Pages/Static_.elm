module Pages.Static_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route { static : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "About"
            , body = [mdToHtml (content route.params.static)]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})


content : String -> String
content key = case key of
    "about" ->
        contentAbout
    "other" ->
        contentOther
    _ ->
        contentOther

contentAbout : String
contentAbout = """
## Shanghai dog gut microbiome

About
"""

contentOther : String
contentOther = """
## Shanghai dog gut microbiome

Other
"""

