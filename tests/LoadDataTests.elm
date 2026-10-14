module LoadDataTests exposing (suite)

import Data.Blobs.MAGs exposing (magsBlob)
import Expect
import LoadData exposing (loadData)
import Set
import Test exposing (Test, describe, test)


header : String
header =
    "id,samples_id,taxonomy,completeness,contamination,genome_size,#16s_rrna,#5s_rrna,#23s_rrna,#trna,nr_contigs,nr_genes,is_representative,binning_tool,assembly_method,comment,file_size,file_sha256"


row : String
row =
    "SHD1_0222,D000,d__Bacteria;p__Campylobacterota;c__Campylobacteria;o__Campylobacterales;f__Campylobacteraceae;g__Campylobacter_D;s__Campylobacter_D upsaliensis,99.99,0.13,1622650,3,0,3,24,1,0,True,SemiBin2,Flye with Polypolish,,0,TODO"


withRow : String -> String
withRow r =
    header ++ "\n" ++ r


suite : Test
suite =
    describe "LoadData.loadData"
        [ test "decodes a row" <|
            \_ ->
                case loadData (withRow row) of
                    Ok [ mag ] ->
                        Expect.all
                            [ \m -> Expect.equal "SHD1_0222" m.id
                            , \m -> Expect.equal [ "D000" ] m.samplesId
                            , \m -> Expect.within (Expect.Absolute 1.0e-9) 99.99 m.completeness
                            , \m -> Expect.within (Expect.Absolute 1.0e-9) 0.13 m.contamination
                            , \m -> Expect.equal 1622650 m.genomeSize
                            , \m -> Expect.equal ( 3, 0, 3 ) ( m.r16sRrna, m.r5sRrna, m.r23sRrna )
                            , \m -> Expect.equal 24 m.trna
                            , \m -> Expect.equal True m.isRepresentative
                            , \m -> Expect.equal "" m.comment
                            , \m -> Expect.equal "TODO" m.fileSha256
                            ]
                            mag

                    other ->
                        Expect.fail ("Expected one MAG, got " ++ Debug.toString other)
        , test "is_representative accepts False" <|
            \_ ->
                loadData (withRow (String.replace ",True," ",False," row))
                    |> Result.map (List.map .isRepresentative)
                    |> Expect.equal (Ok [ False ])
        , test "splits multiple samples" <|
            \_ ->
                loadData (withRow (String.replace "SHD1_0222,D000," "SHD1_0222,\"D000,D001\"," row))
                    |> Result.map (List.map .samplesId)
                    |> Expect.equal (Ok [ [ "D000", "D001" ] ])
        , test "empty taxonomy becomes d__unclassified" <|
            \_ ->
                loadData (withRow "SHD1_9999,D000,,50.0,1.0,1000,0,0,0,10,5,0,False,SemiBin2,Flye with Polypolish,,0,TODO")
                    |> Result.map (List.map .taxonomy)
                    |> Expect.equal (Ok [ "d__unclassified" ])
        , test "taxonomy without d__ prefix gets one" <|
            \_ ->
                loadData (withRow "SHD1_9999,D000,Bacteria;p__Bacillota,50.0,1.0,1000,0,0,0,10,5,0,False,SemiBin2,Flye with Polypolish,,0,TODO")
                    |> Result.map (List.map .taxonomy)
                    |> Expect.equal (Ok [ "d__Bacteria;p__Bacillota" ])
        , test "fails on a non-numeric field" <|
            \_ ->
                loadData (withRow (String.replace ",1622650," ",big," row))
                    |> Expect.err
        , test "fails on a missing column" <|
            \_ ->
                loadData (String.replace ",file_sha256" "" header ++ "\n" ++ String.replace ",TODO" "" row)
                    |> Expect.err
        , describe "the bundled MAGs blob"
            (let
                result =
                    loadData magsBlob

                nrRows =
                    List.length (String.lines magsBlob) - 1
             in
             [ test "decodes every row" <|
                \_ ->
                    case result of
                        Ok mags ->
                            Expect.equal nrRows (List.length mags)

                        Err err ->
                            Expect.fail ("Failed to decode MAGs blob: " ++ Debug.toString err)
             , test "has unique IDs" <|
                \_ ->
                    case result of
                        Ok mags ->
                            Expect.equal (List.length mags) (Set.size (Set.fromList (List.map .id mags)))

                        Err _ ->
                            Expect.fail "Failed to decode MAGs blob"
             ]
            )
        ]
