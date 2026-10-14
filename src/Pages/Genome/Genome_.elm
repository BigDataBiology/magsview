module Pages.Genome.Genome_ exposing (Model, Msg, page)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Effect exposing (Effect)
import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Http
import Json.Decode as D
import Json.Encode as E

import Bootstrap.Grid as Grid
import Bootstrap.Table as Table


import Shared
import Data exposing (mags)
import DataModel exposing (MAG)
import Layouts
import GenomeStats exposing (printableTaxonomy, showTaxon)
import Utils exposing (mkTooltipQuestionMark, showWithCommas, legendItem)
import Data.Info exposing (mkFASTALink, mkENOGLink, mkEMapperSummaryLink)
import GeneSequence
import EMapper exposing (EMapperGene, GeneType(..), parseEMapperTsv)
import GenomeData exposing (MAGData, MicrobeAtlasMatch, decodeMAGData)
import Barrnap exposing (decodeRRnaGenes)
import Cog
import GenomeMap exposing (ContigInfo)

-- INIT

type LoadedDataModel =
    Loaded MAGData
    | LoadError String
    | Waiting

type EMapperDataModel
    = EMapperLoaded (List EMapperGene)
    | EMapperError String
    | EMapperWaiting

type alias GeneSequenceData =
    { dna : String
    , protein : String
    }

type GeneSequenceState
    = NoGeneSelected
    | GeneSequenceLoading EMapperGene
    | GeneSequenceLoaded EMapperGene GeneSequenceData
    | GeneSequenceError EMapperGene String

type RRnaDataModel
    = RRnaLoaded (List EMapperGene)
    | RRnaError String
    | RRnaWaiting

type GeneDisplayFilter
    = ShowAll
    | ShowProteinCoding
    | ShowRRna

type alias Model =
    { magdata : LoadedDataModel
    , emapperData : EMapperDataModel
    , rRnaData : RRnaDataModel
    , expanded : Bool
    , expanded16S : List Int
    , showARGSequences : Bool
    , genomeMap : GenomeMap.State
    , magId : String
    , geneSequence : GeneSequenceState
    , showGeneTable : Bool
    , showDnaSequence : Bool
    , geneFilter : GeneDisplayFilter
    }

type Msg =
    ResultsData (Result Http.Error APIResult)
    | EMapperData (Result Http.Error String)
    | Toggle16SExpanded
    | Expand16S Int
    | ToggleShowARGSequences
    | GenomeMapMsg GenomeMap.Msg
    | SelectAdjacentGene Int
    | GeneClicked EMapperGene
    | GeneSequenceReceived D.Value
    | CloseGeneDetail
    | CopyToClipboard String String
    | ToggleGeneTable
    | ToggleDnaSequence
    | DownloadGeneFasta String
    | RRnaDataReceived D.Value
    | SetGeneFilter GeneDisplayFilter

type APIResult =
    APIError String | APIResultOK MAGData

type alias MightMAG
    = Result String MAG

model0 : String -> Model
model0 magid =
    { magdata = Waiting
    , emapperData = EMapperWaiting
    , rRnaData = RRnaWaiting
    , expanded = False
    , expanded16S = []
    , showARGSequences = False
    , genomeMap = GenomeMap.init
    , magId = magid
    , geneSequence = NoGeneSelected
    , showGeneTable = False
    , showDnaSequence = False
    , geneFilter = ShowAll
    }

cmd0 : String -> Effect Msg
cmd0 magid =
    Effect.batch
        [ Effect.sendCmd
            (Http.get
                { url = "/genome-data/"++ magid ++ ".json"
                , expect = Http.expectJson ResultsData (D.map APIResultOK decodeMAGData)
                }
            )
        , Effect.sendCmd
            (Http.get
                { url = mkEMapperSummaryLink magid
                , expect = Http.expectString EMapperData
                }
            )
        , Effect.sendCmd
            (GeneSequence.requestRRnaGenes
                (E.object [ ( "magId", E.string magid ) ])
            )
        ]

page : Shared.Model -> Route { genome : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> (model0 route.params.genome, cmd0 route.params.genome)
        , update = update
        , subscriptions = \model -> Sub.batch
            [ GeneSequence.receiveGeneSequence GeneSequenceReceived
            , GeneSequence.receiveRRnaGenes RRnaDataReceived
            , Sub.map GenomeMapMsg (GenomeMap.subscriptions model.genomeMap)
            ]
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

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s -> "Bad URL: " ++ s
        Http.Timeout -> "Timeout"
        Http.NetworkError -> "Network error"
        Http.BadStatus s -> "Bad status: " ++ String.fromInt s
        Http.BadBody s -> "Bad body: " ++ s

update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
    case msg of
        ResultsData r ->
            let magdata = case r of
                    Ok (APIResultOK v) -> Loaded v
                    Ok (APIError e) -> LoadError e
                    Err err -> LoadError (httpErrorToString err)
            in
                ( { model | magdata = magdata }
                , Effect.none
                )
        EMapperData r ->
            let emapperData = case r of
                    Ok tsv -> case parseEMapperTsv tsv of
                        Ok genes -> EMapperLoaded genes
                        Err e -> EMapperError e
                    Err err -> EMapperError (httpErrorToString err)
            in
                ( { model | emapperData = emapperData }, Effect.none )
        Toggle16SExpanded ->
            ( { model | expanded = not model.expanded }, Effect.none )
        Expand16S ix ->
            if List.member ix model.expanded16S then
                ({ model | expanded16S = List.filter (\i -> i /= ix) model.expanded16S }, Effect.none)
            else
                ({ model | expanded16S = ix :: model.expanded16S }, Effect.none)
        ToggleShowARGSequences ->
            ( { model | showARGSequences = not model.showARGSequences }
            , Effect.none
            )

        GenomeMapMsg mapMsg ->
            let
                ( genomeMap, cmd ) = GenomeMap.update mapMsg model.genomeMap
            in
            ( { model | genomeMap = genomeMap }
            , Effect.sendCmd (Cmd.map GenomeMapMsg cmd)
            )

        SelectAdjacentGene step ->
            let
                contigs = displayContigs model
                genes = GenomeMap.geneOrder contigs
                currentIndex =
                    selectedGeneFromState model.geneSequence
                        |> Maybe.andThen (\sel -> indexOf (\g -> g.seqid == sel.seqid) genes)
                nextIndex =
                    case currentIndex of
                        Just ix -> Basics.clamp 0 (List.length genes - 1) (ix + step)
                        Nothing -> if step > 0 then 0 else List.length genes - 1
            in
            case List.head (List.drop nextIndex genes) of
                Just gene ->
                    selectGene gene { model | genomeMap = GenomeMap.scrollToGene contigs gene model.genomeMap }
                Nothing ->
                    ( model, Effect.none )

        ToggleGeneTable ->
            ( { model | showGeneTable = not model.showGeneTable }, Effect.none )

        ToggleDnaSequence ->
            ( { model | showDnaSequence = not model.showDnaSequence }, Effect.none )

        DownloadGeneFasta format ->
            case model.emapperData of
                EMapperLoaded genes ->
                    ( model
                    , Effect.sendCmd
                        (GeneSequence.downloadGeneFasta
                            (E.object
                                [ ( "magId", E.string model.magId )
                                , ( "format", E.string format )
                                , ( "genes", E.list encodeGeneForDownload genes )
                                ]
                            )
                        )
                    )
                _ ->
                    ( model, Effect.none )

        GeneClicked gene ->
            case GenomeMap.consumeDragClick model.genomeMap of
                Just genomeMap ->
                    -- This click ends a drag of the map, it is not a selection
                    ( { model | genomeMap = genomeMap }, Effect.none )
                Nothing ->
                    selectGene gene model

        GeneSequenceReceived value ->
            let
                resultDecoder =
                    D.oneOf
                        [ D.map Ok
                            (D.map2 GeneSequenceData
                                (D.field "dna" D.string)
                                (D.field "protein" D.string)
                            )
                        , D.map Err (D.field "error" D.string)
                        ]

                newState =
                    case model.geneSequence of
                        GeneSequenceLoading gene ->
                            -- Replies for a previously clicked gene can arrive after
                            -- the user has selected another one; ignore them
                            if D.decodeValue (D.field "seqid" D.string) value /= Ok gene.seqid then
                                model.geneSequence

                            else
                                case D.decodeValue resultDecoder value of
                                    Ok (Ok seqData) ->
                                        GeneSequenceLoaded gene seqData

                                    Ok (Err errorMsg) ->
                                        GeneSequenceError gene errorMsg

                                    Err err ->
                                        GeneSequenceError gene (D.errorToString err)

                        _ ->
                            model.geneSequence
            in
            ( { model | geneSequence = newState }, Effect.none )

        CloseGeneDetail ->
            ( { model | geneSequence = NoGeneSelected }, Effect.none )

        CopyToClipboard text buttonId ->
            ( model, Effect.sendCmd (GeneSequence.copyToClipboard
                (E.object
                    [ ("text", E.string text)
                    , ("buttonId", E.string buttonId)
                    ])) )

        RRnaDataReceived value ->
            let
                newRRnaData =
                    case D.decodeValue decodeRRnaGenes value of
                        Ok genes -> RRnaLoaded genes
                        Err _ ->
                            case D.decodeValue (D.field "error" D.string) value of
                                Ok errMsg -> RRnaError errMsg
                                Err e -> RRnaError (D.errorToString e)
            in
            ( { model | rRnaData = newRRnaData }, Effect.none )

        SetGeneFilter filter ->
            ( { model | geneFilter = filter }, Effect.none )



selectGene : EMapperGene -> Model -> ( Model, Effect Msg )
selectGene gene model =
    let
        isRRna = case gene.geneType of
            RRna _ -> True
            ProteinCoding -> False
    in
    ( { model | geneSequence = GeneSequenceLoading gene }
    , Effect.sendCmd
        (GeneSequence.requestGeneSequence
            (E.object
                [ ( "magId", E.string model.magId )
                , ( "contig", E.string gene.contig )
                , ( "start", E.int gene.start )
                , ( "end", E.int gene.end )
                , ( "strand", E.string gene.strand )
                , ( "seqid", E.string gene.seqid )
                , ( "skipTranslation", E.bool isRRna )
                ]
            )
        )
    )


indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf pred xs =
    xs
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, x ) -> pred x)
        |> List.head
        |> Maybe.map Tuple.first


view :
    Shared.Model
    -> Route { genome : String }
    -> Model
    -> View Msg
view sm route model =
        { title = route.params.genome ++ " - Genome details"
        , body =
            [ Html.div []
                (case getMAG route.params.genome of
                    Err err ->
                        [ Html.text ("Error: "++err) ]
                    Ok mag ->
                        showMag model mag
                )
            , Html.a [ HtmlAttr.href "/genomes" ]
                [ Html.text "Back to genomes" ]
            ]
        }

basicTR title value =
    Table.tr []
        [ Table.td []
            [ Html.text title ]
        , Table.td []
            [ Html.text value ]
        ]

showMag : Model -> MAG -> List (Html.Html Msg)
showMag model mag =
    [ Html.h1 []
        [ Html.text ("Genome: " ++ mag.id) ]
    , Grid.simpleRow [
        Grid.col [ ] [
            Html.h3 [] [ Html.text "General QC information" ]
            , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ Table.th []
                            [ Html.text "Property" ]
                        , Table.th []
                            [ Html.text "Value" ]
                        ]
                , tbody = Table.tbody []
                    ([basicTR "Genome ID" mag.id
                    , basicTR "#Contigs" (String.fromInt mag.nrContigs)
                    , basicTR "Genome Size" (showWithCommas mag.genomeSize ++ " bp")
                    , basicTR "Completeness" (String.fromFloat mag.completeness ++ "%")
                    , basicTR "Contamination" (String.fromFloat mag.contamination ++ "%")
                    ] ++ show16S model mag ++ [
                      basicTR "#23s rRNA" (String.fromInt mag.r23sRrna)
                    , basicTR "#5s rRNA" (String.fromInt mag.r5sRrna)
                    , basicTR "#tRNA" (String.fromInt mag.trna)
                ])
        }]
        , Grid.col [ ] [
            Html.h3 [] [ Html.text "Taxonomic classification (GTDB)" ]
            ,Html.div
                [HtmlAttr.style "border-bottom" "2px solid black"]
                (let
                    r : List String -> List (Html.Html Msg)
                    r tax = case tax of
                        [] -> []
                        (x::xs) ->
                            [Html.div [HtmlAttr.style "padding-left" "1em"
                                    , HtmlAttr.style "border-left" "2px solid black"
                                    ]
                                ((showTaxon x)::(r xs))
                            ]
                in r (String.split ";" mag.taxonomy)
                )
            , Html.span []
                (let
                    n = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy)
                            |> List.length
                    rep = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy && m.isRepresentative)
                            |> List.head -- should be at most one
                            |> Maybe.withDefault mag -- if no representative, use this one (but should never happen)
                in
                    if mag.isRepresentative
                        then
                            [Html.span []
                                [ Html.text "This genome is the representative"
                                , if n == 1
                                    then Html.text " (and only) "
                                    else Html.text " "
                                , Html.text "genome for "
                                , Html.em []
                                    [ Html.text (printableTaxonomy mag.taxonomy)
                                    ]
                                , Html.text " in our dataset."
                                , mkTooltipQuestionMark ("The representative genome is the best genome in our data. Species were defined by clustering the MAGs with dRep at 95% average nucleotide identity (ANI).")
                                ]
                            ]
                        else
                            [Html.text "This is ", Html.strong [] [Html.text "not"], Html.text " the representative genome for "
                            , Html.em []
                                [ Html.text (printableTaxonomy mag.taxonomy)
                                ]
                            , Html.text " in our dataset."
                            , Html.text " The representative genome is "
                                , Html.a [HtmlAttr.href ("/genome/" ++ rep.id)]
                                    [ Html.text rep.id ]
                                , Html.text " with "
                                , Html.em [] [Html.text (String.fromFloat rep.completeness ++ "% completeness")]
                                , Html.text " and "
                                , Html.em [] [Html.text (String.fromFloat rep.contamination ++ "% contamination")]
                                , Html.text "."
                            ,mkTooltipQuestionMark ("The representative genome is the best genome in our data in terms of QC stats. Species were defined by clustering the MAGs with dRep at 95% average nucleotide identity (ANI).")
                            ]
                    )
            , Html.p []
                (let
                    n = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy)
                            |> List.length
                in
                    [if n == 1
                        then Html.text ""
                        else Html.a [HtmlAttr.href ("/genomes?taxnav=1&taxonomy="++mag.taxonomy)]
                                [Html.text <|
                                        "A total of " ++ String.fromInt n ++
                                            " genomes of "++ printableTaxonomy mag.taxonomy ++ " are available (click to see all)"
                                ]])
            ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
           [ Html.text "Antibiotic resistance genes" ]
           ,showARGs model mag
           , Html.p []
                [ Html.text "ARGs are predicted using RGI (Resistance Gene Identifier) based on the "
                , Html.a [ HtmlAttr.href "https://card.mcmaster.ca/" ] [ Html.text "Comprehensive Antibiotic Resistance Database (CARD)" ]
                , Html.text ". Only Strict and Perfect hits with at least 90% identity and 90% coverage are shown. Results were mapped to "
                , Html.a [ HtmlAttr.href "https://genepi.food.dtu.dk/resfinder"]
                        [ Html.text "ResFinder" ]
                , Html.text " using "
                , Html.a [ HtmlAttr.href "https://doi.org/10.1093/bioinformatics/btaf173" ]
                        [ Html.text "argNorm" ]
                , Html.text "."
                ]
        ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
            [ Html.text "Genome organisation" ]
        , showGenomeMap model
        ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
            [ Html.text "Download data" ]
        , Html.ol []
            [ Html.li []
                [ Html.a
                    [HtmlAttr.href (mkFASTALink mag.id)]
                    [Html.text "FASTA file (sequence)" ]
                ]
            , Html.li []
                [ Html.a
                    [HtmlAttr.href (mkENOGLink mag.id)]
                    [Html.text "Eggnog-mapper annotations" ]
                ]
            , Html.li []
                [ Html.text "RGI predictions (antibiotic resistance genes)" ]
            , Html.li []
                [ Html.a
                    [ HtmlAttr.href "#"
                    , HE.onClick (DownloadGeneFasta "faa")
                    ]
                    [ Html.text "Predicted protein sequences (FAA)" ]
                , case model.emapperData of
                    EMapperLoaded _ -> Html.text ""
                    EMapperWaiting -> Html.text " (loading gene data...)"
                    EMapperError _ -> Html.text " (gene data unavailable)"
                ]
            , Html.li []
                [ Html.a
                    [ HtmlAttr.href "#"
                    , HE.onClick (DownloadGeneFasta "fna")
                    ]
                    [ Html.text "Predicted gene nucleotide sequences (FNA)" ]
                , case model.emapperData of
                    EMapperLoaded _ -> Html.text ""
                    EMapperWaiting -> Html.text " (loading gene data...)"
                    EMapperError _ -> Html.text " (gene data unavailable)"
                ]
            ]
        ]]
    ]


microbeAtlasBaseURL : String
microbeAtlasBaseURL =
    "https://microbeatlas.org/taxon?taxon_id="

showSingle16s : Model -> Int -> MicrobeAtlasMatch -> Html.Html Msg
showSingle16s model ix m =
    Html.div [ HtmlAttr.style "padding-left" "1em"
                , HtmlAttr.style "margin-bottom" "1em"
                , HtmlAttr.style "border-left" "2px solid black"
                , HtmlAttr.style "position" "relative" -- create a containing block for the absolute positioned index later
                ]
            [ if List.member ix model.expanded16S
                then
                    Html.p [HtmlAttr.class "sequence"]
                        [ Html.text m.seq]
                else
                    Html.p [HtmlAttr.class "sequence", HE.onClick (Expand16S ix)]
                        [ Html.text <| String.slice 0 60 m.seq ++ "..." ]
            , Html.p []
                [Html.text "Maps to microbe atlas OTU: "
                , Html.a [ HtmlAttr.href (microbeAtlasBaseURL ++ m.otu), HtmlAttr.class "microbeAtlasLink" ]
                    [ Html.text m.otu ]
                ]
            , Html.div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "left" "0px"
                , HtmlAttr.style "transform" "translateX(-120%)"
                , HtmlAttr.style "top" "0px"
                , HtmlAttr.style "background-color" "#333"
                , HtmlAttr.style "color" "#ccc"
                , HtmlAttr.style "padding" "0.2em 0.5em"
                ]
                [ Html.text (String.fromInt (ix + 1))
                ]
            ]

show16S : Model -> MAG -> List (Table.Row Msg)
show16S model mag =
    case model.magdata of
        Waiting ->
            [basicTR "waiting for data..." ""]
        LoadError e ->
            [basicTR "Data load error" e]
        Loaded magdata ->
            if model.expanded
                then
                [ Table.tr []
                    [ Table.td []
                        [ Html.h5 [HtmlAttr.style "color" "black"]
                            [ Html.text "16s rRNA" ]
                        , Html.p [HtmlAttr.style "padding-top" "3em"
                                , HtmlAttr.style "margin-right" "1.5em"
                                , HtmlAttr.style "font-style" "italic"
                                ]
                            [ Html.text
                                """Microbe Atlas (by the von Mering group) is a
                                database of 16S amplicon sequences and where
                                they are found in the environment. For details,
                                see """
                            , Html.a [HtmlAttr.href "https://doi.org/10.1016/j.cell.2026.01.021"]
                                [ Html.text "(Matias Rodrigues et al., 2026)"]
                            , Html.text "."
                            ]

                        , Html.p [HtmlAttr.style "margin-right" "1.5em"
                                , HtmlAttr.style "font-style" "italic"
                                ]
                            [ Html.text
                                """Following the links on the right will take you to the
                                Microbe Atlas database where you can find information about
                                where these sequences are found in other environments."""
                            ]
                        ]
                    , Table.td []
                        [ Html.div []
                            (magdata.microbeAtlas
                                |> List.indexedMap (showSingle16s model)
                            )
                        ]
                    ]
                ]
                else
                [ Table.tr [Table.rowAttr <| HE.onClick Toggle16SExpanded]
                    [ Table.td [ ]
                        [ Html.span [HE.onClick Toggle16SExpanded ]
                            [ Html.text "#16s rRNA "
                            , Html.a [HtmlAttr.href "#"
                                     ,HE.onClick Toggle16SExpanded]
                                [Html.text "(click to see details & matches)" ]
                            ]
                        ]
                    , Table.td []
                        [ Html.text <| String.fromInt mag.r16sRrna ++ " matches" ]
                    ]
                ]

showARGs : Model -> MAG -> Html.Html Msg
showARGs model mag =
    case model.magdata of
        Waiting ->
            Html.p [] [Html.text "Waiting for data..."]
        LoadError e ->
            Html.p [] [Html.text <| "Data load error: " ++ e]
        Loaded magdata ->
            if List.isEmpty magdata.argData then
                Html.p [] [Html.text "No ARGs found for this genome."]
            else
                Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead = Table.simpleThead
                                        [ Table.th []
                                            [ Html.text "Sequence"
                                            , Html.span [HE.onClick ToggleShowARGSequences]
                                                [ Html.text <| (if model.showARGSequences then " (collapse)" else " (expand)")
                                                ]
                                            ]
                                        , Table.th []
                                            [ Html.text "ARG Name" ]
                                        , Table.th []
                                            [ Html.text "Match category (RGI)" ]
                                        , Table.th []
                                            [ Html.text "Match ID (%)" ]
                                        , Table.th []
                                            [ Html.text "Fraction Match (%)" ]
                                        , Table.th []
                                            [ Html.text "Drug Class(es)" ]
                                        , Table.th []
                                            [ Html.text "In ResFinder?"
                                            , mkTooltipQuestionMark
                                                ("The ResFinder database focuses on clinically relevant ARGs.")
                                            ]
                                        ]
                , tbody = Table.tbody []
                        (magdata.argData
                            |> List.map (\arg ->
                                    Table.tr []
                                        [Table.td [] [Html.p [HtmlAttr.class "sequence"]
                                            [Html.text <| (if model.showARGSequences then arg.seq else String.slice 0 30 arg.seq ++ "...")]]
                                        ,Table.td [] [Html.text <| arg.argName ++ " "
                                                     ,Html.a [HtmlAttr.href <| "https://card.mcmaster.ca/" ++ arg.aro]
                                                        [Html.text <| "(" ++ arg.aro ++ ")"]]
                                        ,Table.td [] [Html.text arg.cutOff]
                                        ,Table.td [] [Html.text <| String.fromFloat arg.matchID]
                                        ,Table.td [] [Html.text <| String.fromFloat arg.fractionMatch]
                                        ,Table.td [] [Html.text arg.drugClass]
                                        ,Table.td [] [Html.text (if arg.inResfinder then "✔" else "✘")]
                                        ])
                            )
                }


-- EMAPPER GENE DOWNLOAD

encodeGeneForDownload : EMapperGene -> E.Value
encodeGeneForDownload gene =
    E.object
        [ ( "seqid", E.string gene.seqid )
        , ( "contig", E.string gene.contig )
        , ( "start", E.int gene.start )
        , ( "end", E.int gene.end )
        , ( "strand", E.string gene.strand )
        , ( "cogCategory", E.string gene.cogCategory )
        , ( "preferredName", E.string gene.preferredName )
        , ( "keggKo", E.list E.string gene.keggKo )
        ]


-- GENOME MAP VISUALIZATION

selectedGeneFromState : GeneSequenceState -> Maybe EMapperGene
selectedGeneFromState state =
    case state of
        NoGeneSelected -> Nothing
        GeneSequenceLoading gene -> Just gene
        GeneSequenceLoaded gene _ -> Just gene
        GeneSequenceError gene _ -> Just gene


filterGenes : GeneDisplayFilter -> List EMapperGene -> List EMapperGene
filterGenes filter genes =
    case filter of
        ShowAll -> genes
        ShowProteinCoding -> List.filter (\g -> g.geneType == ProteinCoding) genes
        ShowRRna -> List.filter (\g -> g.geneType /= ProteinCoding) genes

loadedRRnaGenes : Model -> List EMapperGene
loadedRRnaGenes model =
    case model.rRnaData of
        RRnaLoaded genes -> genes
        _ -> []

{-| All contigs drawn on the map, with only the genes that pass the filter.
-}
displayContigs : Model -> List ContigInfo
displayContigs model =
    case model.emapperData of
        EMapperLoaded proteinGenes ->
            GenomeMap.groupByContig (proteinGenes ++ loadedRRnaGenes model)
                |> List.map (\c -> { c | genes = filterGenes model.geneFilter c.genes })
        _ ->
            []


genomeMapConfig : GenomeMap.Config Msg
genomeMapConfig =
    { toMsg = GenomeMapMsg
    , onGeneClick = GeneClicked
    , onSelectAdjacent = SelectAdjacentGene
    , onClose = CloseGeneDetail
    }


showGenomeMap : Model -> Html.Html Msg
showGenomeMap model =
    case model.emapperData of
        EMapperWaiting ->
            Html.p [] [ Html.text "Loading genome map..." ]
        EMapperError e ->
            Html.p [] [ Html.text ("Could not load genome map: " ++ e) ]
        EMapperLoaded proteinGenes ->
            let
                rRnaGenes = loadedRRnaGenes model
                allGenes = proteinGenes ++ rRnaGenes
                contigs = displayContigs model
                displayGenes = filterGenes model.geneFilter allGenes
                selectedGene = selectedGeneFromState model.geneSequence
            in
            Html.div []
                [ GenomeMap.viewControls genomeMapConfig model.genomeMap contigs
                , geneFilterControls model.geneFilter (not (List.isEmpty rRnaGenes))
                , GenomeMap.view genomeMapConfig model.genomeMap selectedGene contigs
                , GenomeMap.viewHelp
                , renderGeneDetail model.showDnaSequence allGenes model.geneSequence
                , GenomeMap.viewLegend displayGenes
                , Cog.viewSummary proteinGenes
                , Html.div [ HtmlAttr.style "margin-top" "1em" ]
                    [ Html.button
                        [ HE.onClick ToggleGeneTable
                        , HtmlAttr.class "btn btn-outline-secondary btn-sm"
                        ]
                        [ Html.text
                            (if model.showGeneTable
                                then "Hide gene table"
                                else "Show all genes"
                            )
                        ]
                    ]
                , if model.showGeneTable
                    then renderGeneTable displayGenes
                    else Html.text ""
                , Html.p [ HtmlAttr.style "margin-top" "1em"
                         , HtmlAttr.style "font-size" "0.9em"
                         , HtmlAttr.style "color" "#666"
                         ]
                    [ Html.text "Gene predictions and functional annotations were generated using "
                    , Html.a [ HtmlAttr.href "https://github.com/eggnogdb/eggnog-mapper"
                             , HtmlAttr.target "_blank"
                             ]
                        [ Html.text "eggNOG-mapper" ]
                    , Html.text " ("
                    , Html.a [ HtmlAttr.href "https://doi.org/10.1093/molbev/msab293"
                             , HtmlAttr.target "_blank"
                             ]
                        [ Html.text "Cantalapiedra et al., 2021" ]
                    , Html.text "). rRNA genes predicted using "
                    , Html.a [ HtmlAttr.href "https://github.com/tseemann/barrnap"
                             , HtmlAttr.target "_blank"
                             ]
                        [ Html.text "Barrnap" ]
                    , Html.text "."
                    ]
                ]



geneFilterControls : GeneDisplayFilter -> Bool -> Html.Html Msg
geneFilterControls current hasRRna =
    if not hasRRna then
        Html.text ""
    else
        Html.div
            [ HtmlAttr.style "display" "flex"
            , HtmlAttr.style "align-items" "center"
            , HtmlAttr.style "gap" "0.5em"
            , HtmlAttr.style "margin-bottom" "0.5em"
            ]
            [ Html.span
                [ HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "color" "#555"
                ]
                [ Html.text "Show:" ]
            , GenomeMap.buttonGroup
                [ filterButton "All" ShowAll current
                , filterButton "Protein coding" ShowProteinCoding current
                , filterButton "rRNA" ShowRRna current
                ]
            ]
filterButton : String -> GeneDisplayFilter -> GeneDisplayFilter -> Html.Html Msg
filterButton label filter current =
    Html.button
        [ HE.onClick (SetGeneFilter filter)
        , HtmlAttr.style "padding" "0.3em 0.7em"
        , HtmlAttr.style "font-size" "0.85em"
        , HtmlAttr.style "cursor" "pointer"
        , HtmlAttr.style "border" "none"
        , HtmlAttr.style "background" (if filter == current then "#e3f2fd" else "#fff")
        , HtmlAttr.style "color" (if filter == current then "#1565c0" else "#555")
        , HtmlAttr.style "font-weight" (if filter == current then "600" else "normal")
        , HtmlAttr.style "transition" "background 0.15s"
        ]
        [ Html.text label ]


renderGeneDetail : Bool -> List EMapperGene -> GeneSequenceState -> Html.Html Msg
renderGeneDetail showDna allGenes state =
    let
        panel gene content =
            Html.div [ HtmlAttr.class "gene-detail-panel" ]
                ([ geneDetailHeader gene
                 , GenomeMap.viewNeighborhood GeneClicked gene allGenes
                 , geneAnnotationInfo gene
                 ]
                    ++ content
                )
    in
    case state of
        NoGeneSelected ->
            Html.text ""

        GeneSequenceLoading gene ->
            panel gene
                [ Html.p [] [ Html.text "Loading sequence..." ] ]

        GeneSequenceError gene errMsg ->
            panel gene
                [ Html.p [ HtmlAttr.style "color" "#c00" ]
                    [ Html.text ("Error: " ++ errMsg) ]
                ]

        GeneSequenceLoaded gene seqData ->
            let
                isRRna = case gene.geneType of
                    RRna _ -> True
                    ProteinCoding -> False
            in
            panel gene
                [ Html.div []
                    (( if isRRna then
                        []
                      else
                        [ Html.div [ HtmlAttr.style "display" "flex"
                                   , HtmlAttr.style "align-items" "baseline"
                                   , HtmlAttr.style "gap" "0.5em"
                                   ]
                            [ Html.h4 [ HtmlAttr.style "margin" "0.5em 0 0.25em 0" ]
                                [ Html.text "Protein sequence"
                                , Html.span [ HtmlAttr.style "font-weight" "normal"
                                            , HtmlAttr.style "font-size" "0.85em"
                                            , HtmlAttr.style "color" "#666"
                                            , HtmlAttr.style "margin-left" "0.5em"
                                            ]
                                    [ Html.text ("(" ++ showWithCommas (String.length seqData.protein) ++ " aa, translation table 11)") ]
                                ]
                            , copyButton "copy-protein" seqData.protein
                            ]
                        , Html.div [ HtmlAttr.class "sequence" ]
                            (coloredProtein seqData.protein)
                        , aaLegend
                        ]
                    )
                    ++ [ Html.div [ HtmlAttr.style "display" "flex"
                                  , HtmlAttr.style "align-items" "baseline"
                                  , HtmlAttr.style "gap" "0.5em"
                                  ]
                            [ Html.h4 [ HtmlAttr.style "margin" "0.5em 0 0.25em 0"
                                      , HtmlAttr.style "cursor" (if isRRna then "default" else "pointer")
                                      , if isRRna then HtmlAttr.class "" else HE.onClick ToggleDnaSequence
                                      ]
                                [ if isRRna then
                                    Html.text ""
                                  else
                                    Html.text (if showDna then "\u{25BC} " else "\u{25B6} ")
                                , Html.text "DNA sequence"
                                , Html.span [ HtmlAttr.style "font-weight" "normal"
                                            , HtmlAttr.style "font-size" "0.85em"
                                            , HtmlAttr.style "color" "#666"
                                            , HtmlAttr.style "margin-left" "0.5em"
                                            ]
                                    [ Html.text ("(" ++ showWithCommas (String.length seqData.dna) ++ " bp)") ]
                                ]
                            , if showDna || isRRna then copyButton "copy-dna" seqData.dna else Html.text ""
                            ]
                       , if showDna || isRRna then
                            Html.p [ HtmlAttr.class "sequence" ]
                                [ Html.text seqData.dna ]
                         else
                            Html.text ""
                       ]
                    )
                ]


aaGroupColor : Char -> String
aaGroupColor aa =
    case aa of
        -- Nonpolar/hydrophobic
        'G' -> "#f9e79f"
        'A' -> "#f9e79f"
        'V' -> "#f9e79f"
        'L' -> "#f9e79f"
        'I' -> "#f9e79f"
        'P' -> "#f9e79f"
        'F' -> "#f9e79f"
        'M' -> "#f9e79f"
        'W' -> "#f9e79f"
        -- Polar/uncharged
        'S' -> "#abebc6"
        'T' -> "#abebc6"
        'C' -> "#abebc6"
        'Y' -> "#abebc6"
        'N' -> "#abebc6"
        'Q' -> "#abebc6"
        -- Positively charged
        'K' -> "#aed6f1"
        'R' -> "#aed6f1"
        'H' -> "#aed6f1"
        -- Negatively charged
        'D' -> "#f5b7b1"
        'E' -> "#f5b7b1"
        -- Stop
        '*' -> "#d5d8dc"
        _ -> "transparent"


coloredProtein : String -> List (Html.Html msg)
coloredProtein protein =
    protein
        |> String.toList
        |> List.map (\aa ->
            Html.span
                [ HtmlAttr.style "background-color" (aaGroupColor aa) ]
                [ Html.text (String.fromChar aa) ]
        )


aaLegend : Html.Html msg
aaLegend =
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-wrap" "wrap"
        , HtmlAttr.style "gap" "0.3em 1em"
        , HtmlAttr.style "font-size" "0.8em"
        , HtmlAttr.style "margin-top" "0.3em"
        ]
        (List.map (\( color, label ) -> legendItem color label)
            [ ("#f9e79f", "Nonpolar (G, A, V, L, I, P, F, M, W)")
            , ("#abebc6", "Polar (S, T, C, Y, N, Q)")
            , ("#aed6f1", "Positive (K, R, H)")
            , ("#f5b7b1", "Negative (D, E)")
            , ("#d5d8dc", "Stop (*)")
            ]
        )


geneAnnotationInfo : EMapperGene -> Html.Html Msg
geneAnnotationInfo gene =
    let
        row label content =
            Html.tr []
                [ Html.td [ HtmlAttr.style "padding" "0.15em 0.75em 0.15em 0"
                           , HtmlAttr.style "color" "#666"
                           , HtmlAttr.style "vertical-align" "top"
                           , HtmlAttr.style "white-space" "nowrap"
                           ]
                    [ Html.text label ]
                , Html.td [ HtmlAttr.style "padding" "0.15em 0" ]
                    content
                ]
        typeRow =
            case gene.geneType of
                RRna rtype ->
                    [ row "Type"
                        [ Html.span [ HtmlAttr.style "color" (GenomeMap.rRnaColor rtype) ]
                            [ Html.text (String.replace "_" " " rtype) ]
                        ]
                    ]
                ProteinCoding ->
                    []
        cogRow =
            if String.isEmpty gene.cogCategory then
                []
            else
                [ row "COG category"
                    [ Html.text (gene.cogCategory ++ " — " ++ Cog.description (Cog.primaryCategory gene)) ]
                ]
        keggKoRow =
            if List.isEmpty gene.keggKo then
                []
            else
                [ row "KEGG orthology"
                    (List.intersperse (Html.text ", ")
                        (List.map keggKoLink gene.keggKo))
                ]
        keggModuleRow =
            if List.isEmpty gene.keggModule then
                []
            else
                [ row "KEGG module"
                    (List.intersperse (Html.text ", ")
                        (List.map keggModuleLink gene.keggModule))
                ]
        rows = typeRow ++ cogRow ++ keggKoRow ++ keggModuleRow
    in
    if List.isEmpty rows then
        Html.text ""
    else
        Html.table [ HtmlAttr.style "margin" "0.5em 0"
                   , HtmlAttr.style "font-size" "0.9em"
                   ]
            [ Html.tbody [] rows ]


keggKoLink : String -> Html.Html msg
keggKoLink ko =
    let
        koId = String.replace "ko:" "" ko
    in
    Html.a [ HtmlAttr.href ("https://www.genome.jp/dbget-bin/www_bget?" ++ koId)
           , HtmlAttr.target "_blank"
           ]
        [ Html.text ko ]


keggModuleLink : String -> Html.Html msg
keggModuleLink m =
    Html.a [ HtmlAttr.href ("https://www.genome.jp/dbget-bin/www_bget?" ++ m)
           , HtmlAttr.target "_blank"
           ]
        [ Html.text m ]


copyButton : String -> String -> Html.Html Msg
copyButton buttonId text =
    Html.button
        [ HtmlAttr.id buttonId
        , HE.onClick (CopyToClipboard text buttonId)
        , HtmlAttr.class "copy-btn"
        ]
        [ Html.text "\u{1F4CB}" ]


geneDetailHeader : EMapperGene -> Html.Html Msg
geneDetailHeader gene =
    Html.div [ HtmlAttr.style "display" "flex"
             , HtmlAttr.style "justify-content" "space-between"
             , HtmlAttr.style "align-items" "center"
             ]
        [ Html.h3 [ HtmlAttr.style "margin" "0" ]
            [ Html.text
                (case gene.geneType of
                    RRna rtype ->
                        String.replace "_" " " rtype
                    ProteinCoding ->
                        if String.isEmpty gene.preferredName
                            then gene.seqid
                            else gene.preferredName ++ " (" ++ gene.seqid ++ ")"
                )
            ]
        , Html.span [ HtmlAttr.style "font-size" "0.85em"
                    , HtmlAttr.style "color" "#666"
                    , HtmlAttr.style "margin-left" "1em"
                    ]
            [ Html.text (gene.contig
                ++ " : " ++ showWithCommas gene.start
                ++ "-" ++ showWithCommas gene.end
                ++ " [" ++ gene.strand ++ "]"
                )
            ]
        , Html.button
            [ HE.onClick CloseGeneDetail
            , HtmlAttr.style "border" "none"
            , HtmlAttr.style "background" "none"
            , HtmlAttr.style "font-size" "1.2em"
            , HtmlAttr.style "cursor" "pointer"
            , HtmlAttr.style "color" "#666"
            , HtmlAttr.style "padding" "0.2em 0.5em"
            ]
            [ Html.text "\u{2715}" ]
        ]


renderGeneTable : List EMapperGene -> Html.Html Msg
renderGeneTable genes =
    Html.div [ HtmlAttr.style "margin-top" "1em"
             , HtmlAttr.style "max-height" "600px"
             , HtmlAttr.style "overflow-y" "auto"
             ]
        [ Html.table [ HtmlAttr.class "table table-sm table-striped"
                     , HtmlAttr.style "font-size" "0.85em"
                     ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Gene ID" ]
                    , Html.th [] [ Html.text "Type" ]
                    , Html.th [] [ Html.text "Contig" ]
                    , Html.th [] [ Html.text "Start" ]
                    , Html.th [] [ Html.text "End" ]
                    , Html.th [] [ Html.text "Strand" ]
                    , Html.th [] [ Html.text "Size (bp)" ]
                    , Html.th [] [ Html.text "Gene name" ]
                    , Html.th [] [ Html.text "COG" ]
                    , Html.th [] [ Html.text "KEGG KO" ]
                    , Html.th [] [ Html.text "KEGG Module" ]
                    ]
                ]
            , Html.tbody []
                (genes |> List.map (\gene ->
                    Html.tr []
                        [ Html.td [] [ Html.text gene.seqid ]
                        , Html.td []
                            [ case gene.geneType of
                                ProteinCoding -> Html.text "CDS"
                                RRna rtype -> Html.span [ HtmlAttr.style "color" (GenomeMap.rRnaColor rtype) ] [ Html.text "rRNA" ]
                            ]
                        , Html.td [] [ Html.text gene.contig ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas gene.start) ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas gene.end) ]
                        , Html.td [] [ Html.text gene.strand ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas (gene.end - gene.start + 1)) ]
                        , Html.td []
                            [ Html.text
                                (if String.isEmpty gene.preferredName
                                    then "-"
                                    else gene.preferredName
                                )
                            ]
                        , Html.td []
                            [ Html.span [ HtmlAttr.style "color" (Cog.color (Cog.primaryCategory gene)) ]
                                [ Html.text
                                    (if String.isEmpty gene.cogCategory
                                        then "-"
                                        else gene.cogCategory
                                    )
                                ]
                            ]
                        , Html.td []
                            (if List.isEmpty gene.keggKo
                                then [ Html.text "-" ]
                                else gene.keggKo
                                    |> List.map keggKoLink
                                    |> List.intersperse (Html.text ", ")
                            )
                        , Html.td []
                            (if List.isEmpty gene.keggModule
                                then [ Html.text "-" ]
                                else gene.keggModule
                                    |> List.map keggModuleLink
                                    |> List.intersperse (Html.text ", ")
                            )
                        ]
                ))
            ]
        ]

