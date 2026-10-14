module GenomeData exposing (ARG, MAGData, MicrobeAtlasMatch, decodeMAGData)

{-| Per-genome data served from `static/genome-data/<MAG_ID>.json`
-}

import Json.Decode as D


type alias MicrobeAtlasMatch =
    { seq : String
    , otu : String
    }


type alias ARG =
    { seq : String
    , argName : String
    , aro : String
    , cutOff : String
    , matchID : Float
    , fractionMatch : Float
    , inResfinder : Bool
    , drugClass : String
    }


type alias MAGData =
    { microbeAtlas : List MicrobeAtlasMatch
    , argData : List ARG
    }


decodeARG : D.Decoder ARG
decodeARG =
    D.map8 ARG
        (D.field "Sequence" D.string)
        (D.field "ArgName" D.string)
        (D.field "ARO" D.string)
        (D.field "Cut_Off" D.string)
        (D.field "Identity" D.float)
        (D.field "Coverage" D.float)
        (D.field "InResfinder" D.bool)
        (D.field "Drug Class" D.string)


decodeMAGData : D.Decoder MAGData
decodeMAGData =
    D.map2 MAGData
        (D.field "16S"
            (D.list
                (D.map2 MicrobeAtlasMatch
                    (D.field "Seq" D.string)
                    (D.field "OTU" D.string)
                )
            )
        )
        (D.field "ARGs" (D.list decodeARG))
