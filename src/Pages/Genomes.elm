module Pages.Genomes exposing (page, Model, Msg)

import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI


import Layouts
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
import Data exposing (mags)
import GenomeStats exposing (simplifyTaxonomy)
import Shared



type SortOrder =
    ById
    | ByTaxonomy
    | ByCompleteness
    | ByContamination
    | ByNrContigs
    | ByGenomeSize

type alias Model =
    { qualityFilter : Maybe String
    , sortOrder : SortOrder
    , repsOnly : Bool
    , taxonomyFilter : String

    , showFullTaxonomy : Bool

    , hovering : List (CI.One MAG CI.Dot)
    }

type Msg =
    SetSortOrder SortOrder
    | SetRepsOnly Bool
    | UpdateTaxonomyFilter String
    | ToggleShowFullTaxonomy
    | OnHover (List (CI.One MAG CI.Dot))





init : Route () -> () -> (Model, Effect Msg)
init route () =
    let
        model0 =
            { qualityFilter = Nothing
            , sortOrder = ById
            , repsOnly = False
            , taxonomyFilter = ""
            , showFullTaxonomy = False
            , hovering = []
            }
        model = case Dict.get "taxonomy" route.query of
            Just taxonomy ->
               { model0
                    | taxonomyFilter = taxonomy
                    , showFullTaxonomy = True
              }
            Nothing -> model0
    in
        ( model
        , Effect.none
        )

page : Shared.Model -> Route () -> Page Model Msg
page _ route =
    Page.new
        { init = init route
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
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
            OnHover hovering ->
              { model | hovering = hovering }
    in
        ( nmodel
        , Effect.none
        )


view :
    Model
    -> View Msg
view model =
    let
        sel = mags
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
                    ByNrContigs ->
                        ("", toFloat t.nrContigs)
                    ByGenomeSize ->
                        ("", toFloat <| -t.genomeSize)
                        )

            |> (if model.repsOnly
                    then List.filter .isRepresentative
                    else identity)
            |> (if String.isEmpty model.taxonomyFilter
                    then identity
                    else List.filter (\t ->
                            String.contains
                                (String.toLower model.taxonomyFilter)
                                (String.toLower t.taxonomy))
                )

        theader sortO h =
                Table.th
                    [ Table.cellAttr <| HE.onClick ( SetSortOrder sortO)
                    ] [ Html.a [HtmlAttr.href "#" ] [ Html.text h ] ]
        maybeSimplifyTaxonomy =
            if model.showFullTaxonomy then
                (\x -> x)
            else
                simplifyTaxonomy
        taxonomyHeader =
            Table.th
                [
                ]
                [ Html.a [HtmlAttr.href "#"
                    , HE.onClick (SetSortOrder ByTaxonomy)
                    ] [ Html.text "Taxonomy (GTDB)" ]
                , Html.a [HtmlAttr.href "#"
                    , HE.onClick ToggleShowFullTaxonomy
                    ] [ Html.text (if model.showFullTaxonomy then " [collapse]" else " [expand]") ]
                ]
    in
        { title = "Genome browser"
        , body =
            [ Html.div []
                [ Html.h1 []
                    [ Html.text "Genomes table" ]
                ]
            , Grid.simpleRow
                (( Grid.col []
                    [Html.h2 [] [Html.text "Filter genomes"]
                    ,Html.p []
                        [ Html.text ("Selected genomes: " ++ (sel |> List.length |> String.fromInt)
                                    ++ " (of " ++ (mags |> List.length |> String.fromInt) ++ ")")]
                    , Html.p []
                        [ Html.text "Representative genomes only: "
                        , InputCheckbox.view
                            [InputCheckbox.toggle, InputCheckbox.small]
                            { value = model.repsOnly
                            , onInput = SetRepsOnly
                            }
                        ]
                    , Html.p []
                        [ Html.text "Taxonomy filter: "
                        , Html.input
                            [ HtmlAttr.type_ "text"
                            , HtmlAttr.placeholder "Enter taxonomy"
                            , HtmlAttr.size 120
                            -- Not sure why this is needed, but it seems to be on Chrome
                            , HtmlAttr.style "min-width" "320px"
                            , HtmlAttr.value model.taxonomyFilter
                            , HE.onInput UpdateTaxonomyFilter
                            ] []
                        ]
                    ]
                )::(viewCharts model sel))
            , Grid.simpleRow [ Grid.col [ ] [Table.table
                { options = [ Table.striped, Table.hover, Table.responsive ]
                , thead =  Table.simpleThead
                    [ theader ById "MAG ID"
                    , theader ByCompleteness "Completeness"
                    , theader ByContamination "Contamination"
                    , theader ByNrContigs "#Contigs"
                    , theader ByGenomeSize "Genome size (Mbp)"
                    , taxonomyHeader
                    ]
                , tbody =
                    sel
                        |> List.map (\t ->
                            Table.tr []
                                [ Table.td [] [ Html.a
                                                    [ HtmlAttr.href ("/genome/"++ t.id)
                                                    , HtmlAttr.style "font-weight"
                                                        (if t.isRepresentative then "bold" else "normal")
                                                    ]
                                                    [ Html.text t.id ]
                                                ]
                                , Table.td [] [ Html.text (t.completeness |> String.fromFloat) ]
                                , Table.td [] [ Html.text (t.contamination |> String.fromFloat) ]
                                , Table.td [] [ Html.text (t.nrContigs |> String.fromInt) ]
                                , Table.td [] [ Html.text (t.genomeSize |> (\s -> toFloat (s // 1000// 10) /100.0) |> String.fromFloat) ]
                                , Table.td [] [ Html.text <| maybeSimplifyTaxonomy t.taxonomy ]
                                ])
                        |> Table.tbody []
                }
            ]]]
        }

viewCharts model sel =
    [ Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "180px"
            ,HtmlAttr.style "height" "180px"
            ]
            [GenomeStats.chartQualitySummary sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "180px"
            ,HtmlAttr.style "height" "180px"
            ]
            [GenomeStats.chartQualityScatter OnHover model.hovering sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "180px"
            ,HtmlAttr.style "height" "180px"
            ]
            [GenomeStats.chartNrContigs sel]
        ]
    ]

