module ZipDownload exposing
    ( DownloadState(..), Bundle
    , start, addFasta
    , fileStem, magMetadataTsv
    )

{-| Bulk download of a set of MAGs as a single zip file (FASTA files plus a
metadata table and a README), shared by the genomes table and the taxonomy
explorer.
-}

import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import File.Download as Download
import Http
import Time
import Zip
import Zip.Entry

import DataModel exposing (MAG)
import Data.Info exposing (mkFASTALink, mkReadmeFile, datasetTag, datasetSlug)


type DownloadState
    = NotStarted
    | Downloading { expected : Int, fetched : Dict String Bytes }
    | DownloadError String


{-| What goes in the zip. `name` is used for the file names, `title` and
`description` for the README.
-}
type alias Bundle =
    { name : String
    , title : String
    , description : String
    , mags : List MAG
    }


{-| Start fetching the FASTA files of all the MAGs. Each reply comes back as
`toMsg magId result` and should be passed on to `addFasta`.
-}
start : (String -> Result Http.Error Bytes -> msg) -> List MAG -> ( DownloadState, Cmd msg )
start toMsg ms =
    ( Downloading { expected = List.length ms, fetched = Dict.empty }
    , ms
        |> List.map (fetchFastaBytes toMsg)
        |> Cmd.batch
    )


{-| Record one fetched FASTA file. Once all of them are in, the second element
is the command that saves the zip file.
-}
addFasta : Bundle -> String -> Result Http.Error Bytes -> DownloadState -> ( DownloadState, Maybe (Cmd msg) )
addFasta bundle magId result downloadState =
    case downloadState of
        Downloading state ->
            case result of
                Ok bytes ->
                    let
                        newFetched = Dict.insert magId bytes state.fetched
                    in
                    if Dict.size newFetched == state.expected then
                        ( NotStarted, Just (buildAndDownloadZip bundle newFetched) )
                    else
                        ( Downloading { state | fetched = newFetched }, Nothing )
                Err _ ->
                    ( DownloadError ("Failed to download " ++ magId), Nothing )
        _ ->
            ( downloadState, Nothing )


fetchFastaBytes : (String -> Result Http.Error Bytes -> msg) -> MAG -> Cmd msg
fetchFastaBytes toMsg mag =
    Http.request
        { method = "GET"
        , headers = []
        , url = mkFASTALink mag.id
        , body = Http.emptyBody
        , expect = Http.expectBytesResponse (toMsg mag.id) handleBytesResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleBytesResponse : Http.Response Bytes -> Result Http.Error Bytes
handleBytesResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)
        Http.Timeout_ ->
            Err Http.Timeout
        Http.NetworkError_ ->
            Err Http.NetworkError
        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)
        Http.GoodStatus_ _ body ->
            Ok body


{-| Base name (without extension) of the zip and metadata files -}
fileStem : Bundle -> String
fileStem bundle =
    datasetTag ++ "_" ++ String.replace " " "_" bundle.name


buildAndDownloadZip : Bundle -> Dict String Bytes -> Cmd msg
buildAndDownloadZip bundle fetchedFiles =
    let
        dir = (datasetSlug ++ "-magsview/")

        stem = fileStem bundle

        entryMeta path =
            { path = path
            , lastModified = ( Time.utc, Time.millisToPosix 0 )
            , comment = Nothing
            }

        textEntry path content =
            content
                |> Bytes.Encode.string
                |> Bytes.Encode.encode
                |> Zip.Entry.store (entryMeta (dir ++ path))

        fastaEntries =
            fetchedFiles
                |> Dict.toList
                |> List.map (\( magId, bytes ) ->
                    Zip.Entry.store (entryMeta (dir ++ magId ++ ".fna.gz")) bytes
                )

        tsvEntry =
            textEntry (stem ++ ".metadata.tsv") (magMetadataTsv bundle.mags)

        readmeEntry =
            textEntry "README.md" (mkReadmeFile bundle.title bundle.description stem)

        zip =
            Zip.fromEntries (readmeEntry :: tsvEntry :: fastaEntries)
    in
    Download.bytes (stem ++ ".zip") "application/zip" (Zip.toBytes zip)


magMetadataTsv : List MAG -> String
magMetadataTsv ms =
    let
        header =
            "mag_id\ttaxonomy\tcompleteness\tcontamination\tgenome_size\tnr_contigs\tnr_genes\tis_representative"

        row mag =
            String.join "\t"
                [ mag.id
                , mag.taxonomy
                , String.fromFloat mag.completeness
                , String.fromFloat mag.contamination
                , String.fromInt mag.genomeSize
                , String.fromInt mag.nrContigs
                , String.fromInt mag.nrGenes
                , if mag.isRepresentative then "True" else "False"
                ]
    in
    (header :: List.map row ms)
        |> String.join "\n"
