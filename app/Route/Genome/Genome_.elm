module Route.Genome.Genome_ exposing (ActionData, Data, Model, Msg, route)

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
import Shared exposing (loadMAGs)
import Effect exposing (Effect)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)

type SortOrder =
    ById
    | ByTaxonomy
    | ByCompleteness
    | ByContamination

type alias Model =
    { qualityFilter : Maybe String
    , sortOrder : SortOrder
    , repsOnly : Bool
    , taxonomyFilter : String

    , showFullTaxonomy : Bool
    }

model0 =
    { qualityFilter = Nothing
    , sortOrder = ById
    , repsOnly = False
    , taxonomyFilter = ""
    , showFullTaxonomy = False
    }

type Msg =
    SetSortOrder SortOrder
    | SetRepsOnly Bool
    | UpdateTaxonomyFilter String
    | ToggleShowFullTaxonomy

type alias RouteParams =
    { genome : String }


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRender
        { head = head
        , data = data
        , pages = loadMAGs
                    |> BackendTask.map (.mags >> List.map (\m -> { genome = m.id }))
        }
        |> RouteBuilder.buildWithLocalState
        { init = \_ _ -> (model0, Effect.none)
        , subscriptions = (\rp u sm m -> Sub.none)
        , update = update
        , view = view
        }


type alias Data =
    Maybe MAG

type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    loadMAGs
        |> BackendTask.map .mags
        |> BackendTask.map (List.filter (\m -> m.id == routeParams.genome) >> List.head)

update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> (Model, Effect Msg)
update _ sm msg model =
    let
        nmodel = case msg of
            SetSortOrder order ->
                { model | sortOrder = order }
            SetRepsOnly ro ->
                { model | repsOnly = ro }
            UpdateTaxonomyFilter filter ->
                { model | taxonomyFilter = filter }
            ToggleShowFullTaxonomy ->
                { model | showFullTaxonomy = not model.showFullTaxonomy }
    in
        ( nmodel
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
        , title = "Genome collection browser"
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
                (case app.data of
                    Nothing ->
                        [ Html.text "Genome not found" ]
                    Just mag ->
                        showMag mag
                )
            ]
        }
showMag : MAG -> List (Html.Html (PagesMsg Msg))
showMag mag =
    [ Html.h1 []
        [ Html.text ("Genome: " ++ mag.id) ]
    , Html.p []
        [ Html.text ("Taxonomy: " ++ mag.taxonomy) ]
    , Html.p []
        [ Html.text ("Completeness: " ++ String.fromFloat mag.completeness) ]
    , Html.p []
        [ Html.text ("Contamination: " ++ String.fromFloat mag.contamination) ]
    , Html.p []
        [ Html.text ("Is Representative: " ++ (if mag.isRepresentative then "Yes" else "No")) ]
    ]
