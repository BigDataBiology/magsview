module EMapperTests exposing (suite)

import EMapper exposing (GeneType(..), parseEMapperTsv)
import Expect
import Test exposing (Test, describe, test)


header : String
header =
    "seqid\tcontig\tstart\tend\tstrand\tCOG_category\tPreferred_name\tKEGG_ko\tKEGG_Module"


annotatedRow : String
annotatedRow =
    "SHD1_0006_1_381\tSHD1_0006_1\t464328\t465473\t+\tE\tbioF\tko:K00639,ko:K00652\tM00123,M00573,M00577"


unannotatedRow : String
unannotatedRow =
    "SHD1_0006_1_4\tSHD1_0006_1\t2216\t3124\t-\t-\t-\t-\t-"


suite : Test
suite =
    describe "EMapper.parseEMapperTsv"
        [ test "parses an annotated gene" <|
            \_ ->
                parseEMapperTsv (header ++ "\n" ++ annotatedRow)
                    |> Expect.equal
                        (Ok
                            [ { seqid = "SHD1_0006_1_381"
                              , contig = "SHD1_0006_1"
                              , start = 464328
                              , end = 465473
                              , strand = "+"
                              , cogCategory = "E"
                              , preferredName = "bioF"
                              , keggKo = [ "ko:K00639", "ko:K00652" ]
                              , keggModule = [ "M00123", "M00573", "M00577" ]
                              , geneType = ProteinCoding
                              }
                            ]
                        )
        , test "\"-\" means no annotation" <|
            \_ ->
                parseEMapperTsv (header ++ "\n" ++ unannotatedRow)
                    |> Result.map (List.map (\g -> ( g.cogCategory, g.preferredName, ( g.keggKo, g.keggModule ) )))
                    |> Expect.equal (Ok [ ( "", "", ( [], [] ) ) ])
        , test "KEGG columns are optional" <|
            \_ ->
                parseEMapperTsv (header ++ "\nSHD1_0006_1_1\tSHD1_0006_1\t3\t560\t-\tL\t-")
                    |> Result.map (List.map (\g -> ( g.keggKo, g.keggModule )))
                    |> Expect.equal (Ok [ ( [], [] ) ])
        , test "keeps row order and skips blank lines" <|
            \_ ->
                parseEMapperTsv (header ++ "\n" ++ annotatedRow ++ "\n\n" ++ unannotatedRow ++ "\n")
                    |> Result.map (List.map .seqid)
                    |> Expect.equal (Ok [ "SHD1_0006_1_381", "SHD1_0006_1_4" ])
        , test "skips malformed rows" <|
            \_ ->
                parseEMapperTsv
                    (String.join "\n"
                        [ header
                        , "too\tfew\tcolumns"
                        , "SHD1_0006_1_2\tSHD1_0006_1\tstart\t1797\t-\tT\t-\t-\t-"
                        , unannotatedRow
                        ]
                    )
                    |> Result.map (List.map .seqid)
                    |> Expect.equal (Ok [ "SHD1_0006_1_4" ])
        , test "empty input is an error" <|
            \_ ->
                parseEMapperTsv "\n  \n"
                    |> Expect.equal (Err "Empty TSV")
        , test "header only is an error" <|
            \_ ->
                parseEMapperTsv header
                    |> Expect.equal (Err "No valid data rows found")
        ]
