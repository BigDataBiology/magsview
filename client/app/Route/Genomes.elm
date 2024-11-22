module Route.Genomes exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask
import Effect
import ErrorPage
import FatalError
import Head
import Html exposing (..)
import Html.Attributes as Hattr exposing (..)
import PagesMsg
import RouteBuilder
import Server.Request
import Server.Response
import Shared
import UrlPath
import View
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)

type Model = Failure | Loading | Success QualityCounts

type Msg
    = Waiting | GotQualityCounts (Result Http.Error QualityCounts)

type alias RouteParams =
    {}
    
route : RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.serverRender
        { data = data
        , action = action
        , head = head
        } |> RouteBuilder.buildWithLocalState
                     { view = view
                     , init = init
                     , update = update
                     , subscriptions = subscriptions
                     }


init :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> ( Model, Effect.Effect Msg )
init app shared =
    ( Loading, Effect.Cmd getQualityCounts )


update :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect.Effect Msg )
update app shared msg model =
    case msg of
        Waiting ->
            (Loading, Effect.Cmd getQualityCounts)
        GotQualityCounts result ->
            case result of 
                Ok qualityCounts ->
                    (Success qualityCounts, Effect.none)
                Err err -> 
                    (Failure, Effect.none)


subscriptions :
    RouteParams -> UrlPath.UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none


type alias Data =
    {}


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Request
    -> BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage)
data routeParams request =
    BackendTask.succeed (Server.Response.render {})


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View.View (PagesMsg.PagesMsg Msg)
view app shared model =
    { title = "MagsView | Genomes", body = [ 
        div [Hattr.class "genomesPage_aboutSection"][ h1 [] [text """Explore Genomes"""]],
        dashboard model
    ] }


action :
    RouteParams
    -> Server.Request.Request
    -> BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage)
action routeParams request =
    BackendTask.succeed (Server.Response.render {})
    
type alias QualityCounts = 
    { low: Int
    , medium: Int
    , high: Int
    }

dashboard : Model -> Html msg
dashboard model =
    div [Hattr.class "genomesPage_qualityCountsDashboard"]
        [ h2 [] [text "Quality Counts"]
        , viewQualityCounts model
        ]

viewQualityCounts : Model -> Html msg
viewQualityCounts model =
    case model of
        Failure -> div [] [ text "Could not load Quality Counts. Please Refresh."]
        Loading -> text "Loading..."
        Success qualityCounts ->
            ul []
                [ li [] [text ("Low Quality Genomes: " ++ (String.fromInt qualityCounts.low))]
                , li [] [text ("Medium Quality Genomes: " ++ (String.fromInt qualityCounts.medium))]
                , li [] [text ("High Quality Genomes: " ++ (String.fromInt qualityCounts.high))]
                ]

getQualityCounts : Cmd Msg
getQualityCounts =
    Http.get
        { url="../../public/qualitycounts.json"
        , expect = Http.expectJson GotQualityCounts qualityCountsDecoder
        } 

qualityCountsDecoder : Decoder QualityCounts
qualityCountsDecoder =
    map3 QualityCounts 
        (field "low" int)
        (field "medium" int)
        (field "high" int)
