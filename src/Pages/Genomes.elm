module Pages.Genomes exposing (page, Model, Msg)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)


import Shared
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
import Layouts

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


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = \_ ->
            ( model0
            , Effect.none
            )
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
                        )
            |> (if model.repsOnly
                    then List.filter .isRepresentative
                    else identity)
            |> (if String.isEmpty model.taxonomyFilter
                    then identity
                    else List.filter (\t ->
                            String.contains model.taxonomyFilter
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
        simplifyTaxonomy t =
            String.split ";" t
                |> List.reverse
                |> List.filter (\s -> String.length s > 3)
                |> List.head
                |> Maybe.withDefault ""
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
                    [ Html.text "Genome browser" ]
                , Html.p []
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
                        , HtmlAttr.value model.taxonomyFilter
                        , HE.onInput UpdateTaxonomyFilter
                        ] []
                    ]
                ]
            , Grid.simpleRow [ Grid.col [ ] [Table.table
                { options = [ Table.striped, Table.hover, Table.responsive ]
                , thead =  Table.simpleThead
                    [ theader ById "MAG ID"
                    , taxonomyHeader
                    , theader ByCompleteness "Completeness"
                    , theader ByContamination "Contamination"
                    ]
                , tbody =
                    sel
                        |> List.map (\t ->
                            Table.tr []
                                [ Table.td [] [ Html.a
                                                    [
                                                        HtmlAttr.href ("/genome/"++ t.id)
                                                    ]
                                                    [ Html.text t.id ]
                                                ]
                                , Table.td [] [ Html.text <| maybeSimplifyTaxonomy t.taxonomy ]
                                , Table.td [] [ Html.text (t.completeness |> String.fromFloat) ]
                                , Table.td [] [ Html.text (t.contamination |> String.fromFloat) ]
                                ])
                        |> Table.tbody []
                }
            ]]]
        }
