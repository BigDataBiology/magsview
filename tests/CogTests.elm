module CogTests exposing (suite)

import Cog
import EMapper exposing (EMapperGene, GeneType(..))
import Expect
import Test exposing (Test, describe, test)


gene : String -> GeneType -> EMapperGene
gene cog geneType =
    { seqid = "g"
    , contig = "c"
    , start = 1
    , end = 100
    , strand = "+"
    , cogCategory = cog
    , preferredName = ""
    , keggKo = []
    , keggModule = []
    , geneType = geneType
    }


suite : Test
suite =
    describe "Cog"
        [ test "primaryCategory takes the first of several categories" <|
            \_ ->
                Cog.primaryCategory (gene "EH" ProteinCoding)
                    |> Expect.equal "E"
        , test "summarize sorts by count and puts unassigned genes last" <|
            \_ ->
                Cog.summarize
                    [ gene "" ProteinCoding
                    , gene "J" ProteinCoding
                    , gene "EH" ProteinCoding
                    , gene "E" ProteinCoding
                    , gene "" ProteinCoding
                    , gene "" ProteinCoding
                    ]
                    |> Expect.equal
                        { rows =
                            [ { category = "E", count = 2 }
                            , { category = "J", count = 1 }
                            , { category = "", count = 3 }
                            ]
                        , assignedTotal = 3
                        , totalCds = 6
                        }
        , test "summarize ignores rRNA genes" <|
            \_ ->
                Cog.summarize [ gene "" (RRna "16S_rRNA"), gene "K" ProteinCoding ]
                    |> Expect.equal
                        { rows = [ { category = "K", count = 1 } ]
                        , assignedTotal = 1
                        , totalCds = 1
                        }
        , test "percentageText rounds to one decimal" <|
            \_ ->
                List.map (\( c, t ) -> Cog.percentageText c t) [ ( 1, 3 ), ( 1, 8 ), ( 3, 3 ), ( 1, 0 ) ]
                    |> Expect.equal [ "33.3 %", "12.5 %", "100.0 %", "-" ]
        ]
