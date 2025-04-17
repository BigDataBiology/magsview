module Route.Other exposing
    ( Model, Msg, RouteParams, route, Data, ActionData
    )

{-|
@docs Model, Msg, RouteParams, route, Data, ActionData
-}


import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import BackendTask.File
import Effect
import ErrorPage
import Head
import Html
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Server.Request
import Server.Response
import Shared
import UrlPath
import View

import SiteMarkdown exposing (mdToHtml)

type alias Model =
    {}


type alias Msg
    = ()


type alias RouteParams =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }

data : BackendTask FatalError Data
data =
    BackendTask.File.bodyWithoutFrontmatter "content/other.md"
        |> BackendTask.allowFatal
        |> BackendTask.map (\content -> { content = content })


type alias Data =
    { content : String }


type alias ActionData =
    {}



head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> View.View (PagesMsg ())
view app shared =
    { title = "Other", body = [mdToHtml app.data.content] }
