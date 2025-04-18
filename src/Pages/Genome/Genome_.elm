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
import GenomeStats exposing (taxonomyLast, printableTaxonomy)

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

showWithCommas : Int -> String
showWithCommas n =
    let
        addCommas s =
            if String.length s <= 3
                then s
            else (addCommas (String.slice 0 (String.length s - 3) s)) ++ "," ++ String.slice (String.length s - 3) (String.length s) s
    in
        addCommas (String.fromInt n)

showMag : MAG -> List (Html.Html Msg)
showMag mag =
    [ Html.h1 []
        [ Html.text ("Genome: " ++ mag.id) ]
    , Html.h2 []
        [ Html.text "MAG Information" ]
    , Grid.simpleRow [ Grid.col [ ] [
        Table.table
            { options = [ Table.striped, Table.hover, Table.responsive ]
            , thead =  Table.simpleThead
                [ Table.th []
                    [ Html.text "Property" ]
                , Table.th []
                    [ Html.text "Value" ]
                ]
        , tbody = Table.tbody []
            (let
                basicTR title value =
                    Table.tr []
                        [ Table.td []
                            [ Html.text title ]
                        , Table.td []
                            [ Html.text value ]
                        ]
            in
                [basicTR "Genome ID" mag.id
                , Table.tr []
                    [ Table.td []
                        [Html.text "Taxonomy (GTDB)"]
                    , Table.td []
                        [Html.div
                            [HtmlAttr.style "border-bottom" "2px solid black"]
                            (let
                                r : List String -> List (Html.Html Msg)
                                r tax = case tax of
                                    [] -> []
                                    (x::xs) ->
                                        [Html.div [HtmlAttr.style "padding-left" "1em"
                                                , HtmlAttr.style "border-left" "2px solid black"
                                                ]
                                            ((Html.text x)::(r xs))
                                        ]
                            in r (String.split ";" mag.taxonomy)
                            )
                        ]
                    ]
                , Table.tr []
                    [ Table.td []
                        [Html.text "Is Representative"]
                    , Table.td []
                        ((Html.text (if mag.isRepresentative then "Yes" else "No"))
                        ::
                            (let
                                n = mags
                                        |> List.filter (\m -> m.taxonomy == mag.taxonomy)
                                        |> List.length
                            in
                                [if n == 1
                                    then Html.text <| " (only genome of "++printableTaxonomy mag.taxonomy++")"
                                    else Html.a [HtmlAttr.href ("/genomes?taxonomy="++ taxonomyLast mag.taxonomy)]
                                            [Html.text <|
                                                    " (a total of " ++ String.fromInt n ++
                                                        " genomes of "++ printableTaxonomy mag.taxonomy ++ " available, click to see all)"
                                            ]
                                ]
                        ))
                    ]
                , basicTR "#Contigs" (String.fromInt mag.nrContigs)
                , basicTR "Genome Size (bp)" (showWithCommas mag.genomeSize)
                , basicTR "Completeness (%)" (String.fromFloat mag.completeness)
                , basicTR "Contamination (%)" (String.fromFloat mag.contamination)
                , basicTR "#16s rRNA" (String.fromInt mag.r16sRrna)
                , basicTR "#23s rRNA" (String.fromInt mag.r23sRrna)
                , basicTR "#5s rRNA" (String.fromInt mag.r5sRrna)
                , basicTR "#tRNA" (String.fromInt mag.trna)
            ])
        }
    ]]]
