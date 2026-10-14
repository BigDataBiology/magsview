module BarrnapTests exposing (suite)

import Barrnap exposing (decodeRRnaGenes, parseHeader)
import EMapper exposing (GeneType(..))
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)


location : String -> Maybe ( String, ( Int, Int ), String )
location header =
    parseHeader header
        |> Maybe.map (\g -> ( g.contig, ( g.start, g.end ), g.strand ))


suite : Test
suite =
    describe "Barrnap"
        [ describe "parseHeader"
            [ test "parses a forward-strand header" <|
                \_ ->
                    parseHeader ">16S_rRNA::SHD1_0006_1:848776-850300(+)"
                        |> Expect.equal
                            (Just
                                { seqid = "16S_rRNA::SHD1_0006_1:848776-850300(+)"
                                , contig = "SHD1_0006_1"
                                , start = 848776
                                , end = 850300
                                , strand = "+"
                                , cogCategory = ""
                                , preferredName = "16S rRNA"
                                , keggKo = []
                                , keggModule = []
                                , geneType = RRna "16S_rRNA"
                                }
                            )
            , test "parses a reverse-strand header" <|
                \_ ->
                    location ">5S_rRNA::SHD1_0006_1:92619-94143(-)"
                        |> Expect.equal (Just ( "SHD1_0006_1", ( 92619, 94143 ), "-" ))
            , test "contig names may contain colons" <|
                \_ ->
                    location ">23S_rRNA::k141:12:5-10(+)"
                        |> Expect.equal (Just ( "k141:12", ( 5, 10 ), "+" ))
            , test "ignores trailing whitespace and CR" <|
                \_ ->
                    location ">16S_rRNA::c1:1-2(+) \u{000D}"
                        |> Expect.equal (Just ( "c1", ( 1, 2 ), "+" ))
            , describe "rejects malformed headers" <|
                List.map
                    (\h -> test (Debug.toString h) <| \_ -> parseHeader h |> Expect.equal Nothing)
                    [ ""
                    , "16S_rRNA::c1:1-2(+)"
                    , ">16S_rRNA:c1:1-2(+)"
                    , ">16S rRNA::c1:1-2(+)"
                    , ">::c1:1-2(+)"
                    , ">16S_rRNA:::1-2(+)"
                    , ">16S_rRNA::c1:1-2(.)"
                    , ">16S_rRNA::c1:1-2"
                    , ">16S_rRNA::c1:1..2(+)"
                    , ">16S_rRNA::c1:+1-2(+)"
                    , ">16S_rRNA::c1:1-2-3(+)"
                    , ">16S_rRNA::c1:-2(+)"
                    , ">16S_rRNA::c1:1-2(+)x"
                    ]
            ]
        , describe "decodeRRnaGenes"
            [ test "parses headers, skipping malformed ones" <|
                \_ ->
                    E.object
                        [ ( "headers"
                          , E.list E.string
                                [ ">16S_rRNA::c1:1-1500(+)"
                                , ">garbage"
                                , ">5S_rRNA::c2:10-120(-)"
                                ]
                          )
                        ]
                        |> D.decodeValue decodeRRnaGenes
                        |> Result.map (List.map .geneType)
                        |> Expect.equal (Ok [ RRna "16S_rRNA", RRna "5S_rRNA" ])
            , test "an error payload does not decode" <|
                \_ ->
                    E.object [ ( "error", E.string "Failed to fetch rRNA data: 404" ) ]
                        |> D.decodeValue decodeRRnaGenes
                        |> Expect.err
            ]
        ]
