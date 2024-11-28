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
import BackendTask.File as File
import Server.Response
import Csv.Decode as Decode

import Bootstrap.Table as Table


type alias Model =
    {}

type Msg
    = NoOp


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
    ( {}, Effect.none )

update :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect.Effect Msg )
update app shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions :
    RouteParams -> UrlPath.UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none

type alias Genome =
    { id : String
    , taxonomy : String
    , completeness : String
    , contamination : String
    , num_16s_rrna : String
    , num_5s_rrna : String
    , num_23s_rrna : String
    , num_trna : String
    , genome_size : String
    , is_representative : String
    }
    
genomeDecoder : Decode.Decoder Genome
genomeDecoder =
    Decode.into Genome
        |> Decode.pipeline (Decode.field "Bin ID" Decode.string)
        |> Decode.pipeline (Decode.field "Classification" Decode.string)
        |> Decode.pipeline (Decode.field "Completeness" Decode.string)
        |> Decode.pipeline (Decode.field "Contamination" Decode.string)
        |> Decode.pipeline (Decode.field "16S rRNA" Decode.string)
        |> Decode.pipeline (Decode.field "5S rRNA" Decode.string)
        |> Decode.pipeline (Decode.field "23S rRNA" Decode.string)
        |> Decode.pipeline (Decode.field "Total tRNAs" Decode.string)
        |> Decode.pipeline (Decode.field "Genome Size" Decode.string)
        |> Decode.pipeline (Decode.field "Representative" Decode.string)
        

type alias Data = String


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Request
    -> BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage)
data routeParams request =
    File.rawFile "magsdata.csv" |> BackendTask.allowFatal |> BackendTask.map Server.Response.render


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
        case Decode.decodeCsv Decode.FieldNamesFromFirstRow genomeDecoder app.data of
            Ok genomesList -> genomesTable genomesList
            Err err -> p [] [text (Debug.toString err)]
    ] }


action :
    RouteParams
    -> Server.Request.Request
    -> BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage)
action routeParams request =
    BackendTask.succeed (Server.Response.render {})

displayGenomeRow : Genome -> Table.Row msg
displayGenomeRow genome =
    Table.tr []
        [ Table.td [] [text genome.id]
        , Table.td [] [text genome.completeness]
        , Table.td [] [text genome.contamination]
        , Table.td [] [text genome.genome_size]
        , Table.td [] [text genome.is_representative]
        ]


genomesTable : List Genome -> Html msg
genomesTable genomesList =
    div [Hattr.class "table"] 
        [ Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =  Table.simpleThead
                [ Table.th [] [ text "ID" ]
                , Table.th [] [ text "Completeness" ]
                , Table.th [] [ text "Contamination" ]
                , Table.th [] [ text "Genome Size" ]
                , Table.th [] [ text "Representative" ]
                ]
            , tbody =
                Table.tbody [] (List.map displayGenomeRow genomesList)
            }
        ]