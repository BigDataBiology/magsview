module GenomeDataTests exposing (suite)

import Expect
import GenomeData exposing (decodeMAGData)
import Json.Decode as D
import Test exposing (Test, describe, test)


arg : String
arg =
    """{"Sequence": "MNHWKST", "ArgName": "Mef(En2)", "Cut_Off": "Strict", "Identity": 99.5, "Coverage": 100.0, "Drug Class": "macrolide antibiotic; lincosamide antibiotic", "ARO": "ARO:3004659", "InResfinder": false}"""


genomeJson : String -> String -> String
genomeJson rrnas args =
    "{\"16S\": [" ++ rrnas ++ "], \"ARGs\": [" ++ args ++ "]}"


match16S : String
match16S =
    """{"Seq": "AGAAGAGTTTGATCC", "OTU": "90_258;96_6438;97_7716;98_9487;99_12584"}"""


suite : Test
suite =
    describe "GenomeData.decodeMAGData"
        [ test "decodes 16S matches and ARGs" <|
            \_ ->
                D.decodeString decodeMAGData (genomeJson match16S arg)
                    |> Expect.equal
                        (Ok
                            { microbeAtlas =
                                [ { seq = "AGAAGAGTTTGATCC", otu = "90_258;96_6438;97_7716;98_9487;99_12584" } ]
                            , argData =
                                [ { seq = "MNHWKST"
                                  , argName = "Mef(En2)"
                                  , aro = "ARO:3004659"
                                  , cutOff = "Strict"
                                  , matchID = 99.5
                                  , fractionMatch = 100.0
                                  , inResfinder = False
                                  , drugClass = "macrolide antibiotic; lincosamide antibiotic"
                                  }
                                ]
                            }
                        )
        , test "empty lists" <|
            \_ ->
                D.decodeString decodeMAGData (genomeJson "" "")
                    |> Expect.equal (Ok { microbeAtlas = [], argData = [] })
        , test "integer percentages are accepted" <|
            \_ ->
                D.decodeString decodeMAGData (genomeJson "" (String.replace "99.5" "100" arg))
                    |> Result.map (.argData >> List.map .matchID)
                    |> Expect.equal (Ok [ 100.0 ])
        , test "missing ARGs key fails" <|
            \_ ->
                D.decodeString decodeMAGData ("{\"16S\": [" ++ match16S ++ "]}")
                    |> Expect.err
        , test "missing ARG field fails" <|
            \_ ->
                D.decodeString decodeMAGData (genomeJson "" (String.replace "\"ARO\": \"ARO:3004659\", " "" arg))
                    |> Expect.err
        , test "string where a number is expected fails" <|
            \_ ->
                D.decodeString decodeMAGData (genomeJson "" (String.replace "99.5" "\"99.5\"" arg))
                    |> Expect.err
        ]
