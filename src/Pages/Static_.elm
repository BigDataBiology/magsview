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
    "manuscript" ->
        contentManuscript
    _ ->
        contentOther

contentAbout : String
contentAbout = """
## Shanghai dog gut microbiome

This was a project led by _Anna Cuscó_ (Fudan University) in the [Big Data Biology Lab](https://big-data-biology.org) led by _Luis Pedro Coelho_. The project is currently being finalized for publication. The data will also be made available at a suitable repository.

Please contact us if you are interested in the data: [anna@big-data-biology.org](mailto:anna@big-data-biology.org) or [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.rorg).
"""

contentOther : String
contentOther = """
## Shanghai dog gut microbiome

Other
"""

contentManuscript : String
contentManuscript = """
## A manuscript is under preparation

Manuscript
"""

