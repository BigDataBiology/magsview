module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import BackendTask exposing (BackendTask)
import BackendTask.File
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)
import Csv.Decode as Decode

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import W.Styles


import DataModel exposing (MAG)
import LoadData exposing (loadData)

template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


type Msg
    = SharedMsg SharedMsg


type alias Data =
    {
        mags : List MAG
    }

type SharedMsg
    = NoOp


type alias Model =
    ()


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init flags maybePagePath =
    ( ()
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : BackendTask FatalError Data
data =
    BackendTask.File.rawFile "content/mags.csv"
        |> BackendTask.allowFatal
        |>  BackendTask.andThen (\csv -> case loadData csv of
            Ok loaded ->
                BackendTask.succeed loaded
            Err err ->
                BackendTask.fail (FatalError.fromString ("Failed to load data: " ++ Decode.errorToString err))
        ) |> BackendTask.map (\m -> { mags = m })

view :
    Data
    ->
        { path : UrlPath
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html msg), title : String }
view sharedData page model toMsg pageView =
    { body =
        [ CDN.stylesheet
        , CDN.fontAwesome
        , W.Styles.globalStyles
        , W.Styles.baseTheme
        , Html.main_ []
            [
                Html.nav []
                [Grid.simpleRow
                    [ Grid.col [] [ Html.a [HtmlAttr.href "/"] [Html.text "Home"]]
                    , Grid.col [] [ Html.a [HtmlAttr.href "/genomes"] [Html.text "Genomes"]]
                    , Grid.col [] [ Html.a [HtmlAttr.href "/taxonomy"] [Html.text "Taxonomy"]]
                    , Grid.col [] [ Html.a [HtmlAttr.href "/other"] [Html.text "Other Data"]]
                    , Grid.col [] [ Html.a [HtmlAttr.href "/about"] [Html.text "About&Contact"]]
                    ]
                ]
            , Html.div []
                (pageView.body)
            , Html.div [HtmlAttr.id "footer"]
                [ Html.p []
                    [ Html.text "For more information about the data, please see "
                    , Html.a
                        [HtmlAttr.href "#"]
                        [ Html.text "(Cuscó et al. 2025)"]
                    ]
                ]
            ]
        ]
    , title = pageView.title
    }
