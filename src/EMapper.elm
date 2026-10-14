module EMapper exposing (EMapperGene, GeneType(..), parseEMapperTsv)

{-| Genes and their eggNOG-mapper annotations, as served in the
`.emapper_summary.tsv` files (see `create-magsviewdata-emapper.py`).
-}


type GeneType
    = ProteinCoding
    | RRna String -- "16S_rRNA", "23S_rRNA", "5S_rRNA"


type alias EMapperGene =
    { seqid : String
    , contig : String
    , start : Int
    , end : Int
    , strand : String
    , cogCategory : String
    , preferredName : String
    , keggKo : List String
    , keggModule : List String
    , geneType : GeneType
    }


{-| Parse the summary TSV. The first non-empty line is the header; rows that
cannot be parsed are skipped.
-}
parseEMapperTsv : String -> Result String (List EMapperGene)
parseEMapperTsv tsv =
    let
        lines =
            String.lines tsv
                |> List.filter (\l -> not (String.isEmpty (String.trim l)))
    in
    case lines of
        [] ->
            Err "Empty TSV"

        _ :: dataLines ->
            let
                parsed =
                    List.filterMap parseTsvLine dataLines
            in
            if List.isEmpty parsed then
                Err "No valid data rows found"

            else
                Ok parsed


parseTsvLine : String -> Maybe EMapperGene
parseTsvLine line =
    let
        splitCommas s =
            if s == "-" then
                []

            else
                String.split "," s
    in
    case String.split "\t" line of
        seqid :: contig :: startStr :: endStr :: strand :: cogCat :: prefName :: rest ->
            case ( String.toInt startStr, String.toInt endStr ) of
                ( Just start, Just end ) ->
                    let
                        keggKo =
                            case rest of
                                ko :: _ ->
                                    splitCommas ko

                                _ ->
                                    []

                        keggModule =
                            case rest of
                                _ :: km :: _ ->
                                    splitCommas km

                                _ ->
                                    []
                    in
                    Just
                        { seqid = seqid
                        , contig = contig
                        , start = start
                        , end = end
                        , strand = strand
                        , cogCategory =
                            if cogCat == "-" then
                                ""

                            else
                                cogCat
                        , preferredName =
                            if prefName == "-" then
                                ""

                            else
                                prefName
                        , keggKo = keggKo
                        , keggModule = keggModule
                        , geneType = ProteinCoding
                        }

                _ ->
                    Nothing

        _ ->
            Nothing
