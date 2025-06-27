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
import W.InputSlider as InputSlider
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)
import Data exposing (mags)
import GenomeStats exposing (taxonomyLast)
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
    , taxonomyUpActive : Bool
    , maxNrContigsStep : Float

    , showFullTaxonomy : Bool

    , hovering : List (CI.One MAG CI.Dot)
    }

type Msg =
    SetSortOrder SortOrder
    | SetRepsOnly Bool
    | UpdateTaxonomyFilter String
    | UpdateMaxNrContigs Float
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
            , taxonomyUpActive = False
            , maxNrContigsStep = 6
            , showFullTaxonomy = False
            , hovering = []
            }
        model1 = case Dict.get "taxonomy" route.query of
            Just taxonomy ->
               { model0
                    | taxonomyFilter = taxonomy
                    , showFullTaxonomy = True
              }
            Nothing -> model0
        model =
            case Dict.get "taxnav" route.query of
                Just "1" ->
                    { model1 | taxonomyUpActive = True }
                _ ->
                    model1
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


step2maxContigs : Float -> Int
step2maxContigs step =
    if step < 0.5
        then 1
    else if step < 1.5
        then 5
    else if step < 2.5
        then 10
    else if step < 3.5
        then 20
    else if step < 4.5
        then 50
    else if step < 5.5
        then 100
    else -1

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
                { model | taxonomyFilter = filter
                        , taxonomyUpActive = upOneLevel filter /= filter
                        }
            UpdateMaxNrContigs step ->
                { model | maxNrContigsStep = step }
            ToggleShowFullTaxonomy ->
                { model | showFullTaxonomy = not model.showFullTaxonomy }
            OnHover hovering ->
              { model | hovering = hovering }
    in
        ( nmodel
        , Effect.none
        )


last : List a -> Maybe a
last xs =
    case List.reverse xs of
        [] ->
            Nothing
        x :: _ ->
            Just x

upOneLevel : String -> String
upOneLevel taxonomy =
    case last (String.indexes ";" taxonomy) of
        Nothing ->
            taxonomy
        Just i ->
            if i > 0 then
                String.left i taxonomy
            else
                taxonomy

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
            |> (if maxNrContigs > 0
                    then List.filter (\t -> t.nrContigs <= maxNrContigs)
                    else identity)

        maxNrContigs =
            step2maxContigs model.maxNrContigsStep
        theader sortO h =
                Table.th
                    [ Table.cellAttr <| HE.onClick ( SetSortOrder sortO)
                    ] [ Html.a [HtmlAttr.href "#" ] [ Html.text h ] ]
        maybeSimplifyTaxonomy =
            if model.showFullTaxonomy then
                identity
            else
                taxonomyLast
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
                (( Grid.col [Col.lg4]
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
                        , Html.span []
                            (if model.taxonomyUpActive
                                then
                                    [ Html.a [HE.onClick (UpdateTaxonomyFilter <| upOneLevel model.taxonomyFilter), HtmlAttr.href "#"]
                                        [ Html.text "[go up one level]" ]
                                    ]
                                else
                                    [ ]
                            )
                        , Input.text
                            [ Input.placeholder "Enter taxonomy"
                            , Input.value model.taxonomyFilter
                            , Input.onInput UpdateTaxonomyFilter
                            ]
                        ]
                    , Html.p []
                        [ Html.text "Max # contigs: "
                        , Html.text (
                            if maxNrContigs < 0
                                then "no limit"
                            else if maxNrContigs == 1
                                then "1 (single contig only)"
                            else (String.fromInt maxNrContigs ++ " contigs"))
                        , InputSlider.view []
                            { min = 0
                            , max = 6
                            , step = 1
                            , value = model.maxNrContigsStep
                            , onInput = UpdateMaxNrContigs
                            }
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
            [HtmlAttr.style "width" "100px"
            ,HtmlAttr.style "height" "210px"
            ,HtmlAttr.style "margin-left" "50px"
            ]
            [GenomeStats.chartQualitySummary sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "210px"
            ,HtmlAttr.style "height" "210px"
            ]
            [GenomeStats.chartQualityScatter OnHover model.hovering sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "210px"
            ,HtmlAttr.style "height" "210px"
            ]
            [GenomeStats.chartNrContigs sel]
        ]
    ]

