module Route.Genomes exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Http
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html
import Json.Decode as Decode
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatefulRoute, StatelessRoute)
import Server.Request as Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Effect
import View exposing (View)


type alias Model =
    { qualityFilter : Maybe String
    }


type alias Msg =
    ()


type alias RouteParams =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRender
        { head = head
        , data = data
        , pages = BackendTask.succeed [{}]
        }
        |> RouteBuilder.buildWithLocalState
        { init = \_ _ -> ({ qualityFilter = Nothing }, Effect.none)
        , subscriptions = (\rp u sm m -> Sub.none)
        , update = (\_ sm msg m -> (m, Effect.none))
        , view = view
        }


type alias Data = Shared.Data

type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams = Shared.template.data


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    { title = "Genome browser"
    , body =
        [ Html.div []
            [ Html.text
                ("Hello " ++ (app.data |> List.length |>  String.fromInt))
            ]
        ]
    }
