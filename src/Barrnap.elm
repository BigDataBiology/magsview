module Barrnap exposing (decodeRRnaGenes, parseHeader)

{-| rRNA genes predicted by Barrnap, read from the FASTA headers of the
`<MAG_ID>_ribosomal.fna.gz` files. Headers have the form

    >{type}::{contig}:{start}-{end}({strand})

for example `>16S_rRNA::SHD1_0006_k141_1:100-1650(+)`.

-}

import EMapper exposing (EMapperGene, GeneType(..))
import Json.Decode as D


{-| Decode the port payload `{ "headers": [...] }`, skipping headers that do
not parse
-}
decodeRRnaGenes : D.Decoder (List EMapperGene)
decodeRRnaGenes =
    D.field "headers" (D.list D.string)
        |> D.map (List.filterMap parseHeader)


parseHeader : String -> Maybe EMapperGene
parseHeader line =
    case String.uncons (String.trimRight line) of
        Just ( '>', header ) ->
            splitFirst "::" header
                |> Maybe.andThen
                    (\( rtype, location ) ->
                        if isWord rtype then
                            parseLocation location
                                |> Maybe.map (mkGene rtype)

                        else
                            Nothing
                    )

        _ ->
            Nothing


{-| Parse `{contig}:{start}-{end}({strand})`. The contig name may itself contain
colons.
-}
parseLocation : String -> Maybe { contig : String, start : Int, end : Int, strand : String }
parseLocation location =
    let
        strand =
            if String.endsWith "(+)" location then
                Just "+"

            else if String.endsWith "(-)" location then
                Just "-"

            else
                Nothing
    in
    case ( strand, splitLast ":" (String.dropRight 3 location) ) of
        ( Just s, Just ( contig, range ) ) ->
            case String.split "-" range of
                [ startStr, endStr ] ->
                    case ( toNat startStr, toNat endStr ) of
                        ( Just start, Just end ) ->
                            if String.isEmpty contig then
                                Nothing

                            else
                                Just { contig = contig, start = start, end = end, strand = s }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


mkGene : String -> { contig : String, start : Int, end : Int, strand : String } -> EMapperGene
mkGene rtype { contig, start, end, strand } =
    { seqid = rtype ++ "::" ++ contig ++ ":" ++ String.fromInt start ++ "-" ++ String.fromInt end ++ "(" ++ strand ++ ")"
    , contig = contig
    , start = start
    , end = end
    , strand = strand
    , cogCategory = ""
    , preferredName = String.replace "_" " " rtype
    , keggKo = []
    , keggModule = []
    , geneType = RRna rtype
    }


isWord : String -> Bool
isWord s =
    not (String.isEmpty s) && String.all (\c -> Char.isAlphaNum c || c == '_') s


{-| Only digits (`String.toInt` also accepts a leading sign)
-}
toNat : String -> Maybe Int
toNat s =
    if not (String.isEmpty s) && String.all Char.isDigit s then
        String.toInt s

    else
        Nothing


splitFirst : String -> String -> Maybe ( String, String )
splitFirst sep s =
    case String.indexes sep s of
        ix :: _ ->
            Just ( String.left ix s, String.dropLeft (ix + String.length sep) s )

        [] ->
            Nothing


splitLast : String -> String -> Maybe ( String, String )
splitLast sep s =
    case List.reverse (String.indexes sep s) of
        ix :: _ ->
            Just ( String.left ix s, String.dropLeft (ix + String.length sep) s )

        [] ->
            Nothing
