module Pages.Genome.Genome_ exposing (Model, Msg, page)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Effect exposing (Effect)
import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table


import Shared
import Data exposing (mags)
import DataModel exposing (MAG)
import Layouts

-- INIT

type alias Model =
    ()

type alias Msg = ()


type alias MightMAG
    = Result String MAG

page : Shared.Model -> Route { genome : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ((), Effect.none)
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view shared route
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


getMAG : String -> MightMAG
getMAG g =
    mags
        |> List.filter (\m -> m.id == g)
        |> List.head
        |> (\mm -> case mm of
            Just m -> Ok m
            Nothing -> Err ("Genome not found: " ++ g)
        )

update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
        ( model
        , Effect.none
        )

view :
    Shared.Model
    -> Route { genome : String }
    -> Model
    -> View Msg
view sm route model =
        { title = "Genome browser"
        , body =
            [ Html.div []
                (case getMAG route.params.genome of
                    Err err ->
                        [ Html.text ("Error: "++err) ]
                    Ok mag ->
                        showMag mag
                )
            , Html.a [ HtmlAttr.href "/genomes" ]
                [ Html.text "Back to genomes" ]
            ]
        }
showMag : MAG -> List (Html.Html Msg)
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
