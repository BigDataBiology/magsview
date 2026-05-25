module Pages.Healthcheck exposing (page, Model, Msg)

import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Http
import Json.Decode as D
import Task
import Time

import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)

import Bootstrap.Button as Button
import Bootstrap.Table as Table

import Layouts
import Data exposing (mags)
import DataModel exposing (MAG)
import Data.Info exposing
    ( mkFASTALink, mkENOGLink, mkEMapperSummaryLink, mkBarrnapLink
    , datasetName)


maxConcurrent : Int
maxConcurrent = 20


type ResourceKind
    = FastaResource
    | EnogResource
    | EmapperSummaryResource
    | BarrnapResource
    | JsonResource


type Status
    = Pending
    | Success Int
    | Failed String


type SampleSize
    = Random10
    | Random100
    | AllMags


type alias Model =
    { results : Dict ( String, String ) Status
    , queue : List ( String, ResourceKind )
    , inFlight : Int
    , total : Int
    , done : Int
    , sample : SampleSize
    }


type Msg
    = SetSampleSize SampleSize
    | StartCheck
    | CancelCheck
    | GotStartTime Time.Posix
    | GotResult String ResourceKind (Result Http.Error Int)


kindCode : ResourceKind -> String
kindCode k =
    case k of
        FastaResource -> "fasta"
        EnogResource -> "enog"
        EmapperSummaryResource -> "emap"
        BarrnapResource -> "barrnap"
        JsonResource -> "json"


kindFromCode : String -> ResourceKind
kindFromCode c =
    case c of
        "fasta" -> FastaResource
        "enog" -> EnogResource
        "emap" -> EmapperSummaryResource
        "barrnap" -> BarrnapResource
        _ -> JsonResource


kindLabel : ResourceKind -> String
kindLabel k =
    case k of
        FastaResource -> "Genome FASTA (.fna.gz) [HEAD]"
        EnogResource -> "EggNOG annotations (.emapper.annotations.xz) [HEAD]"
        EmapperSummaryResource -> "EggNOG summary (.emapper_summary.tsv) [HEAD]"
        BarrnapResource -> "Barrnap rRNA (_ribosomal.fna.gz) [HEAD]"
        JsonResource -> "Per-genome JSON (16S / ARG) [GET + decode]"


allKinds : List ResourceKind
allKinds =
    [ FastaResource
    , EnogResource
    , EmapperSummaryResource
    , BarrnapResource
    , JsonResource
    ]


resourceUrl : ResourceKind -> String -> String
resourceUrl k magId =
    case k of
        FastaResource -> mkFASTALink magId
        EnogResource -> mkENOGLink magId
        EmapperSummaryResource -> mkEMapperSummaryLink magId
        BarrnapResource -> mkBarrnapLink magId
        JsonResource -> "/genome-data/" ++ magId ++ ".json"


init : () -> ( Model, Effect Msg )
init _ =
    ( { results = Dict.empty
      , queue = []
      , inFlight = 0
      , total = 0
      , done = 0
      , sample = Random10
      }
    , Effect.none
    )


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Main {})


sampleCount : SampleSize -> Int
sampleCount s =
    case s of
        Random10 -> 10
        Random100 -> 100
        AllMags -> List.length mags


isRunning : Model -> Bool
isRunning model =
    model.inFlight > 0 || not (List.isEmpty model.queue)


-- Tag each element with a pseudo-random key derived from `seed`, sort by it,
-- and strip the keys. Good enough for sampling MAGs without depending on
-- elm/random.
shuffleWithSeed : Int -> List a -> List a
shuffleWithSeed seed xs =
    let
        step s =
            modBy 2147483647 (s * 1103515245 + 12345)

        -- We use an accumulator to make the function tail-recursive
        -- and avoid stack overflow on large lists. The fact that the
        -- result is in reverse order doesn't matter since we sort by the keys
        -- anyway.
        tag acc s items =
            case items of
                [] ->
                    acc

                x :: rest ->
                    let
                        s2 = step s
                    in
                    tag ( (s2, x) :: acc ) s2 rest
    in
    xs
        |> tag [] (max 1 seed)
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetSampleSize s ->
            ( { model | sample = s }, Effect.none )

        StartCheck ->
            ( model
            , Effect.sendCmd (Task.perform GotStartTime Time.now)
            )

        CancelCheck ->
            -- Clear the queue. Already-in-flight requests will land normally
            -- (and update `results`), but no new ones will be dispatched.
            ( { model | queue = [] }, Effect.none )

        GotStartTime now ->
            let
                seed = Time.posixToMillis now

                selected =
                    case model.sample of
                        AllMags ->
                            mags

                        _ ->
                            mags
                                |> shuffleWithSeed seed
                                |> List.take (sampleCount model.sample)

                pairs =
                    selected
                        |> List.concatMap
                            (\m -> List.map (\k -> ( m.id, k )) allKinds)

                results0 =
                    pairs
                        |> List.map
                            (\( id, k ) -> ( ( id, kindCode k ), Pending ))
                        |> Dict.fromList

                ( toStart, rest ) =
                    splitAt maxConcurrent pairs

                cmd =
                    toStart
                        |> List.map (\( id, k ) -> checkResource id k)
                        |> Cmd.batch
            in
            ( { model
                | results = results0
                , queue = rest
                , inFlight = List.length toStart
                , total = List.length pairs
                , done = 0
              }
            , Effect.sendCmd cmd
            )

        GotResult magId kind res ->
            let
                status =
                    case res of
                        Ok code -> Success code
                        Err err -> Failed (httpErrorToString err)

                results =
                    Dict.insert ( magId, kindCode kind ) status model.results

                done = model.done + 1

                ( nextStart, queueAfter ) =
                    case model.queue of
                        [] -> ( Nothing, [] )
                        next :: rest -> ( Just next, rest )

                inFlight =
                    model.inFlight - 1 + case nextStart of
                        Nothing -> 0
                        Just _ -> 1

                cmd =
                    case nextStart of
                        Nothing -> Cmd.none
                        Just ( id, k ) -> checkResource id k
            in
            ( { model
                | results = results
                , queue = queueAfter
                , inFlight = inFlight
                , done = done
              }
            , Effect.sendCmd cmd
            )


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s -> "Bad URL: " ++ s
        Http.Timeout -> "Timeout"
        Http.NetworkError -> "Network error (CORS or offline)"
        Http.BadStatus s -> "HTTP " ++ String.fromInt s
        Http.BadBody s -> "Bad body: " ++ s


checkResource : String -> ResourceKind -> Cmd Msg
checkResource magId kind =
    case kind of
        JsonResource ->
            jsonGet magId

        _ ->
            headCheck magId kind


headCheck : String -> ResourceKind -> Cmd Msg
headCheck magId kind =
    Http.request
        { method = "HEAD"
        , headers = []
        , url = resourceUrl kind magId
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse
                (GotResult magId kind)
                handleStatusResponse
        , timeout = Just 30000
        , tracker = Nothing
        }


handleStatusResponse : Http.Response String -> Result Http.Error Int
handleStatusResponse response =
    case response of
        Http.BadUrl_ url -> Err (Http.BadUrl url)
        Http.Timeout_ -> Err Http.Timeout
        Http.NetworkError_ -> Err Http.NetworkError
        Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
        Http.GoodStatus_ metadata _ -> Ok metadata.statusCode


-- Download and decode the per-genome JSON. We only check the top-level
-- structure (the same keys that Genome_.elm reads), so a malformed file is
-- reported as Http.BadBody.
jsonStructureDecoder : D.Decoder ()
jsonStructureDecoder =
    D.map2 (\_ _ -> ())
        (D.field "16S" (D.list D.value))
        (D.field "ARGs" (D.list D.value))


jsonGet : String -> Cmd Msg
jsonGet magId =
    Http.get
        { url = resourceUrl JsonResource magId
        , expect =
            Http.expectJson
                (\r ->
                    GotResult magId JsonResource (Result.map (\_ -> 200) r)
                )
                jsonStructureDecoder
        }



-- VIEW


view : Model -> View Msg
view model =
    { title = datasetName ++ ": resource healthcheck"
    , body =
        [ Html.h1 [] [ Html.text "Resource healthcheck" ]
        , Html.p []
            [ Html.text
                ("Confirms each per-genome resource is reachable. "
                    ++ "External files are checked with HEAD requests "
                    ++ "(no payload download); the per-genome JSON is "
                    ++ "downloaded in full and parsed. At most "
                    ++ String.fromInt maxConcurrent
                    ++ " requests are in flight at a time.")
            ]
        , viewControls model
        , viewProgress model
        , viewSummary model
        , viewFailures model
        ]
    }


viewControls : Model -> Html.Html Msg
viewControls model =
    let
        running = isRunning model

        radio lbl s =
            Html.label [ HtmlAttr.style "margin-right" "1em" ]
                [ Html.input
                    [ HtmlAttr.type_ "radio"
                    , HtmlAttr.checked (model.sample == s)
                    , HE.onClick (SetSampleSize s)
                    , HtmlAttr.disabled running
                    ] []
                , Html.text (" " ++ lbl)
                ]

        actionButton =
            if running then
                Button.button
                    [ Button.danger
                    , Button.onClick CancelCheck
                    ]
                    [ Html.text "Cancel" ]
            else
                Button.button
                    [ Button.primary
                    , Button.onClick StartCheck
                    ]
                    [ Html.text "Start check" ]
    in
    Html.div [ HtmlAttr.style "margin-bottom" "1em" ]
        [ Html.p []
            [ Html.text "Sample: "
            , radio "10 random MAGs" Random10
            , radio "100 random MAGs" Random100
            , radio
                ("All " ++ String.fromInt (List.length mags) ++ " MAGs (slow)")
                AllMags
            ]
        , actionButton
        ]


viewProgress : Model -> Html.Html Msg
viewProgress model =
    if model.total == 0 then
        Html.text ""
    else
        let
            pct =
                if model.total == 0 then
                    0
                else
                    (model.done * 100) // model.total
        in
        Html.p []
            [ Html.text
                ("Progress: "
                    ++ String.fromInt model.done
                    ++ " / "
                    ++ String.fromInt model.total
                    ++ " ("
                    ++ String.fromInt pct
                    ++ "%) — "
                    ++ String.fromInt model.inFlight
                    ++ " in flight, "
                    ++ String.fromInt (List.length model.queue)
                    ++ " queued")
            ]


type alias Counts =
    { ok : Int, failed : Int, pending : Int }


countsByKind : Model -> ResourceKind -> Counts
countsByKind model kind =
    let
        code = kindCode kind

        bump status acc =
            case status of
                Success _ -> { acc | ok = acc.ok + 1 }
                Failed _ -> { acc | failed = acc.failed + 1 }
                Pending -> { acc | pending = acc.pending + 1 }
    in
    Dict.foldl
        (\( _, k ) s acc ->
            if k == code then
                bump s acc
            else
                acc
        )
        { ok = 0, failed = 0, pending = 0 }
        model.results


viewSummary : Model -> Html.Html Msg
viewSummary model =
    if model.total == 0 then
        Html.text ""
    else
        Table.table
            { options = [ Table.striped, Table.small ]
            , thead =
                Table.simpleThead
                    [ Table.th [] [ Html.text "Resource type" ]
                    , Table.th [] [ Html.text "OK" ]
                    , Table.th [] [ Html.text "Failed" ]
                    , Table.th [] [ Html.text "Pending" ]
                    ]
            , tbody =
                Table.tbody []
                    (allKinds
                        |> List.map
                            (\k ->
                                let
                                    c = countsByKind model k
                                in
                                Table.tr []
                                    [ Table.td [] [ Html.text (kindLabel k) ]
                                    , Table.td [] [ Html.text (String.fromInt c.ok) ]
                                    , Table.td
                                        [ Table.cellAttr <|
                                            HtmlAttr.style "color"
                                                (if c.failed > 0 then "red" else "inherit")
                                        ]
                                        [ Html.text (String.fromInt c.failed) ]
                                    , Table.td [] [ Html.text (String.fromInt c.pending) ]
                                    ]
                            )
                    )
            }


viewFailures : Model -> Html.Html Msg
viewFailures model =
    let
        failures =
            Dict.toList model.results
                |> List.filterMap
                    (\( ( id, code ), s ) ->
                        case s of
                            Failed msg -> Just ( id, code, msg )
                            _ -> Nothing
                    )

        shown = List.take 100 failures
    in
    if List.isEmpty failures then
        Html.text ""
    else
        Html.div []
            [ Html.h2 []
                [ Html.text
                    ("Failures ("
                        ++ String.fromInt (List.length failures)
                        ++ ")")
                ]
            , if List.length failures > List.length shown then
                Html.p []
                    [ Html.text
                        ("Showing first "
                            ++ String.fromInt (List.length shown))
                    ]
              else
                Html.text ""
            , Table.table
                { options = [ Table.striped, Table.small, Table.responsive ]
                , thead =
                    Table.simpleThead
                        [ Table.th [] [ Html.text "MAG ID" ]
                        , Table.th [] [ Html.text "Resource" ]
                        , Table.th [] [ Html.text "Error" ]
                        , Table.th [] [ Html.text "URL" ]
                        ]
                , tbody =
                    Table.tbody []
                        (shown
                            |> List.map
                                (\( id, code, msg ) ->
                                    let
                                        k = kindFromCode code
                                        url = resourceUrl k id
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text id ]
                                        , Table.td [] [ Html.text (kindLabel k) ]
                                        , Table.td [] [ Html.text msg ]
                                        , Table.td []
                                            [ Html.a
                                                [ HtmlAttr.href url
                                                , HtmlAttr.target "_blank"
                                                ]
                                                [ Html.text url ]
                                            ]
                                        ]
                                )
                        )
                }
            ]
