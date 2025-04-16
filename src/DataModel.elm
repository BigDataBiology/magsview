module DataModel exposing (..)

{-| This code was automatically generated from the data model in the docs/data-model.md file
    using chatgpt
-}

-- SAMPLE

type alias Sample =
    { id : String
    , externalId : String
    , latitude : Float
    , longitude : Float
    , collectionDate : String
    , microontology : String
    , biomeExpanded : String
    , comment : String
    }


-- MAG

type alias MAG =
    { id : String
    , samplesId : List String
    , taxonomy : String
    , completeness : Float
    , contamination : Float
    , r16sRrna : Int
    , r5sRrna : Int
    , r23sRrna : Int
    , trna : Int
    , nrContigs : Int
    , nrGenes : Int
    , isRepresentative : Bool
    , binningTool : String
    , assemblyMethod : String
    , comment : String
    , fileSize : Int
    , fileSha256 : String
    }


-- FEATURE

type alias Feature =
    { id : String
    , name : String
    , vocabulary : String
    }


-- FEATURE IN GENOME

type alias FeatureInGenome =
    { featureId : String
    , genomeId : String
    , contig : String
    , start : Int
    , end : Int
    , score : Float
    , comment : String
    }

