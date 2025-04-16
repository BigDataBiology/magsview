module Route.Genomes exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Http
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatefulRoute, StatelessRoute)
import Server.Request as Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Effect exposing (Effect)
import View exposing (View)


import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

type SortOrder =
    ById
    | ByTaxonomy
    | ByCompleteness
    | ByContamination

type alias Model =
    { qualityFilter : Maybe String
    , sortOrder : SortOrder
    }

model0 =
    { qualityFilter = Nothing
    , sortOrder = ById
    }

type Msg =
    SetSortOrder SortOrder

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
        { init = \_ _ -> (model0, Effect.none)
        , subscriptions = (\rp u sm m -> Sub.none)
        , update = update
        , view = view
        }


type alias Data = Shared.Data

type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams = Shared.template.data

update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> (Model, Effect Msg)
update _ sm msg model = case msg of
    SetSortOrder order ->
        ( { model | sortOrder = order }
        , Effect.none
        )

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
    let
        sel = app.data
            |> List.sortBy (\t ->
                -- sortBy can receive any comparable value, but it must have a consistent
                -- type, so we use a tuple to sort by either a string or a float
                case model.sortOrder of
                    ById ->
                        (t.id, 0.0)
                    ByTaxonomy ->
                        (t.taxonomy, 0.0)
                    ByCompleteness ->
                        ("", -t.completeness)
                    ByContamination ->
                        ("", t.contamination)
                        )
        theader sortO h =
                Table.th
                    [ Table.cellAttr <| HE.onClick (PagesMsg.fromMsg <| SetSortOrder sortO)
                    ] [ Html.a [HtmlAttr.href "#" ] [ Html.text h ] ]
    in
        { title = "Genome browser"
        , body =
            [ Html.div []
                [ Html.h1 []
                    [ Html.text "Genome browser" ]
                , Html.p []
                    [ Html.text ("Total genomes: " ++ (app.data |> List.length |>  String.fromInt)) ]
                , Html.p []
                    [ Html.text ("Selected genomes: " ++ (sel |> List.length |> String.fromInt)) ]
                ]
            , Table.table
                { options = [ Table.striped, Table.hover, Table.responsive ]
                , thead =  Table.simpleThead
                    [ theader ById "MAG ID"
                    , theader ByTaxonomy "Taxonomy (GTDB)"
                    , theader ByCompleteness "Completeness"
                    , theader ByContamination "Contamination"
                    ]
                , tbody =
                    sel
                        |> List.map (\t ->
                            Table.tr []
                                [ Table.td [] [ Html.text t.id ]
                                , Table.td [] [ Html.text t.taxonomy ]
                                , Table.td [] [ Html.text (t.completeness |> String.fromFloat) ]
                                , Table.td [] [ Html.text (t.contamination |> String.fromFloat) ]
                                ])
                        |> Table.tbody []
                }
            ]
        }
