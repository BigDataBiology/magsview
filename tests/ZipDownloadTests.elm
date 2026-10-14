module ZipDownloadTests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import DataModel exposing (MAG)
import Dict
import Expect
import Http
import Test exposing (Test, describe, test)
import ZipDownload exposing (DownloadState(..))


mag : String -> MAG
mag mid =
    { id = mid
    , samplesId = []
    , taxonomy = "d__Bacteria;p__Bacillota"
    , completeness = 98.5
    , contamination = 0.25
    , genomeSize = 2837883
    , r16sRrna = 1
    , r5sRrna = 1
    , r23sRrna = 1
    , trna = 40
    , nrContigs = 2
    , nrGenes = 2500
    , isRepresentative = True
    , binningTool = ""
    , assemblyMethod = ""
    , comment = ""
    , fileSize = 0
    , fileSha256 = ""
    }


bundle : ZipDownload.Bundle
bundle =
    { name = "Bacillota bacterium"
    , title = "Bacillota bacterium"
    , description = "filtered by taxonomy"
    , mags = [ mag "SHD1_0001", mag "SHD1_0002" ]
    }


someBytes : Bytes
someBytes =
    Bytes.Encode.encode (Bytes.Encode.string ">c1\nACGT\n")


{-| Summary of the result of `addFasta`, as neither `Bytes` nor `Cmd` can be
compared with `==`
-}
summarize : ( DownloadState, Maybe (Cmd msg) ) -> String
summarize ( state, saveZip ) =
    let
        st =
            case state of
                NotStarted ->
                    "NotStarted"

                Downloading { expected, fetched } ->
                    "Downloading " ++ String.fromInt (Dict.size fetched) ++ "/" ++ String.fromInt expected

                DownloadError err ->
                    "DownloadError " ++ err
    in
    case saveZip of
        Just _ ->
            st ++ " +zip"

        Nothing ->
            st


started : DownloadState
started =
    ZipDownload.start (\_ _ -> ()) bundle.mags
        |> Tuple.first


suite : Test
suite =
    describe "ZipDownload"
        [ test "fileStem prefixes the dataset tag and removes spaces" <|
            \_ ->
                ZipDownload.fileStem bundle
                    |> Expect.equal "SHD1_Bacillota_bacterium"
        , test "magMetadataTsv writes a header and one row per MAG" <|
            \_ ->
                ZipDownload.magMetadataTsv [ mag "SHD1_0001" ]
                    |> Expect.equal
                        ("mag_id\ttaxonomy\tcompleteness\tcontamination\tgenome_size\tnr_contigs\tnr_genes\tis_representative\n"
                            ++ "SHD1_0001\td__Bacteria;p__Bacillota\t98.5\t0.25\t2837883\t2\t2500\tTrue"
                        )
        , test "start expects one file per MAG" <|
            \_ ->
                summarize ( started, Nothing )
                    |> Expect.equal "Downloading 0/2"
        , test "addFasta waits for all files before saving the zip" <|
            \_ ->
                ZipDownload.addFasta bundle "SHD1_0001" (Ok someBytes) started
                    |> summarize
                    |> Expect.equal "Downloading 1/2"
        , test "addFasta saves the zip once the last file is in" <|
            \_ ->
                ZipDownload.addFasta bundle "SHD1_0001" (Ok someBytes) started
                    |> Tuple.first
                    |> ZipDownload.addFasta bundle "SHD1_0002" (Ok someBytes)
                    |> summarize
                    |> Expect.equal "NotStarted +zip"
        , test "addFasta reports a failed file" <|
            \_ ->
                ZipDownload.addFasta bundle "SHD1_0002" (Err Http.NetworkError) started
                    |> summarize
                    |> Expect.equal "DownloadError Failed to download SHD1_0002"
        , test "addFasta ignores replies when no download is running" <|
            \_ ->
                ZipDownload.addFasta bundle "SHD1_0001" (Ok someBytes) NotStarted
                    |> summarize
                    |> Expect.equal "NotStarted"
        ]
