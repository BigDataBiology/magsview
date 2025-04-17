module Pages.Home_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "About"
            , body = [mdToHtml content]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})

content : String
content = """
## Shanghai dog gut microbiome

Homepage
"""

