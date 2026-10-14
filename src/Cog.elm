module Cog exposing
    ( SummaryRow
    , Summary
    , allCategories
    , color
    , description
    , primaryCategory
    , summarize
    , percentageText
    , viewSummary
    )

{-| COG functional categories: colours, descriptions and the per-genome
summary of how many protein-coding genes fall in each category.
-}

import Dict
import Html
import Html.Attributes as HtmlAttr

import EMapper exposing (EMapperGene, GeneType(..))
import Utils exposing (showWithCommas)


{-| All single-letter categories in display order; "" is "no COG category".
-}
allCategories : List String
allCategories =
    [ "J", "A", "K", "L", "B", "D", "Y", "V", "T", "M", "N", "U", "O", "C", "G", "E", "F", "H", "I", "P", "Q", "R", "S", "" ]


{-| eggNOG-mapper can assign several categories (e.g. "EH"); the first one is
used as the primary class.
-}
primaryCategory : EMapperGene -> String
primaryCategory gene =
    String.left 1 gene.cogCategory


color : String -> String
color cat =
    case cat of
        "J" -> "#f06292"   -- Translation
        "A" -> "#e91e63"   -- RNA processing
        "K" -> "#ab47bc"   -- Transcription
        "L" -> "#7e57c2"   -- Replication/repair
        "B" -> "#5c6bc0"   -- Chromatin
        "D" -> "#66bb6a"   -- Cell cycle
        "Y" -> "#a5d6a7"   -- Nuclear structure
        "V" -> "#43a047"   -- Defense
        "T" -> "#ff9800"   -- Signal transduction
        "M" -> "#8bc34a"   -- Cell wall
        "N" -> "#00acc1"   -- Cell motility
        "U" -> "#26c6da"   -- Secretion
        "O" -> "#ec407a"   -- Post-translational modification
        "C" -> "#42a5f5"   -- Energy
        "G" -> "#fdd835"   -- Carbohydrate
        "E" -> "#ffb74d"   -- Amino acid
        "F" -> "#9575cd"   -- Nucleotide
        "H" -> "#64b5f6"   -- Coenzyme
        "I" -> "#a1887f"   -- Lipid
        "P" -> "#4db6ac"   -- Inorganic ion
        "Q" -> "#ff7043"   -- Secondary metabolites
        "R" -> "#bdbdbd"   -- General function
        "S" -> "#e0e0e0"   -- Function unknown
        "" -> "#f5f5f5"    -- No COG
        _ -> "#cccccc"


description : String -> String
description cat =
    case cat of
        "J" -> "Translation, ribosomal structure"
        "A" -> "RNA processing and modification"
        "K" -> "Transcription"
        "L" -> "Replication, recombination and repair"
        "B" -> "Chromatin structure"
        "D" -> "Cell cycle control, cell division"
        "Y" -> "Nuclear structure"
        "V" -> "Defense mechanisms"
        "T" -> "Signal transduction"
        "M" -> "Cell wall/membrane biogenesis"
        "N" -> "Cell motility"
        "U" -> "Intracellular trafficking, secretion"
        "O" -> "Post-translational modification"
        "C" -> "Energy production and conversion"
        "G" -> "Carbohydrate transport and metabolism"
        "E" -> "Amino acid transport and metabolism"
        "F" -> "Nucleotide transport and metabolism"
        "H" -> "Coenzyme transport and metabolism"
        "I" -> "Lipid transport and metabolism"
        "P" -> "Inorganic ion transport and metabolism"
        "Q" -> "Secondary metabolites"
        "R" -> "General function prediction only"
        "S" -> "Function unknown"
        "" -> "No COG category"
        _ -> cat


-- SUMMARY


type alias SummaryRow =
    { category : String
    , count : Int
    }


{-| `rows` lists the assigned categories from most to least common, followed
by the unassigned genes (category "") if there are any.
-}
type alias Summary =
    { rows : List SummaryRow
    , assignedTotal : Int
    , totalCds : Int
    }


summarize : List EMapperGene -> Summary
summarize genes =
    let
        proteinGenes =
            List.filter (\g -> g.geneType == ProteinCoding) genes

        rows =
            proteinGenes
                |> List.foldl
                    (\gene acc ->
                        Dict.update
                            (primaryCategory gene)
                            (\count -> Just (Maybe.withDefault 0 count + 1))
                            acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map (\( category, count ) -> { category = category, count = count })

        assignedRows =
            rows
                |> List.filter (\row -> not (String.isEmpty row.category))
                |> List.sortBy (\row -> 0 - row.count)

        unassignedRows =
            List.filter (\row -> String.isEmpty row.category) rows
    in
    { rows = assignedRows ++ unassignedRows
    , assignedTotal = List.sum (List.map .count assignedRows)
    , totalCds = List.length proteinGenes
    }


{-| `count` as a percentage of `total` with one decimal, e.g. "12.5 %".
-}
percentageText : Int -> Int -> String
percentageText count total =
    if total <= 0 then
        "-"

    else
        let
            tenths =
                round ((toFloat count * 1000) / toFloat total)
        in
        String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths) ++ " %"


percentWidth : Int -> Int -> String
percentWidth count total =
    if total <= 0 then
        "0%"

    else
        String.fromFloat ((toFloat count * 100) / toFloat total) ++ "%"


-- VIEW


viewSummary : List EMapperGene -> Html.Html msg
viewSummary genes =
    let
        summary = summarize genes
    in
    if summary.totalCds == 0 then
        Html.text ""

    else
        Html.div
            [ HtmlAttr.style "margin-top" "1em"
            , HtmlAttr.style "border-top" "1px solid #e5e7eb"
            , HtmlAttr.style "padding-top" "1em"
            ]
            [ Html.h3
                [ HtmlAttr.style "font-size" "1.1em"
                , HtmlAttr.style "margin-bottom" "0.25em"
                ]
                [ Html.text "COG category summary" ]
            , Html.p
                [ HtmlAttr.style "font-size" "0.9em"
                , HtmlAttr.style "color" "#555"
                , HtmlAttr.style "margin-bottom" "0.75em"
                ]
                [ Html.text
                    ("Primary COG class for "
                        ++ showWithCommas summary.assignedTotal
                        ++ " annotated CDS"
                    )
                , if summary.assignedTotal == summary.totalCds then
                    Html.text "."

                  else
                    Html.text
                        ("; "
                            ++ showWithCommas (summary.totalCds - summary.assignedTotal)
                            ++ " CDS without a COG category."
                        )
                ]
            , viewCompositionBar summary
            , Html.div
                [ HtmlAttr.style "max-width" "860px"
                , HtmlAttr.style "overflow-x" "auto"
                ]
                [ Html.table
                    [ HtmlAttr.class "table table-sm"
                    , HtmlAttr.style "font-size" "0.9em"
                    , HtmlAttr.style "margin-bottom" "0"
                    ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [] [ Html.text "COG cat." ]
                            , Html.th [] [ Html.text "Description" ]
                            , Html.th [ HtmlAttr.style "text-align" "right" ] [ Html.text "CDS count" ]
                            , Html.th [] [ Html.text "Share" ]
                            , Html.th [ HtmlAttr.style "text-align" "right" ] [ Html.text "% of annotated" ]
                            ]
                        ]
                    , Html.tbody []
                        (List.map (viewSummaryRow summary.assignedTotal) summary.rows)
                    ]
                ]
            ]


viewCompositionBar : Summary -> Html.Html msg
viewCompositionBar summary =
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "height" "14px"
        , HtmlAttr.style "max-width" "860px"
        , HtmlAttr.style "margin-bottom" "0.75em"
        , HtmlAttr.style "border" "1px solid #d8dee7"
        , HtmlAttr.style "border-radius" "4px"
        , HtmlAttr.style "overflow" "hidden"
        ]
        (summary.rows
            |> List.map
                (\row ->
                    Html.div
                        [ HtmlAttr.style "width" (percentWidth row.count summary.totalCds)
                        , HtmlAttr.style "background-color" (color row.category)
                        , HtmlAttr.title
                            ((if String.isEmpty row.category then
                                description row.category

                              else
                                row.category ++ ": " ++ description row.category
                             )
                                ++ " ("
                                ++ showWithCommas row.count
                                ++ " CDS)"
                            )
                        ]
                        []
                )
        )


viewSummaryRow : Int -> SummaryRow -> Html.Html msg
viewSummaryRow assignedTotal row =
    let
        unassigned = String.isEmpty row.category
    in
    Html.tr []
        [ Html.td []
            [ Html.span
                [ HtmlAttr.style "display" "inline-flex"
                , HtmlAttr.style "align-items" "center"
                , HtmlAttr.style "gap" "0.45em"
                ]
                [ Html.span
                    [ HtmlAttr.style "display" "inline-block"
                    , HtmlAttr.style "width" "0.85em"
                    , HtmlAttr.style "height" "0.85em"
                    , HtmlAttr.style "border" "1px solid #8a94a3"
                    , HtmlAttr.style "background-color" (color row.category)
                    ]
                    []
                , Html.text (if unassigned then "-" else row.category)
                ]
            ]
        , Html.td [] [ Html.text (description row.category) ]
        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas row.count) ]
        , Html.td []
            [ Html.div
                [ HtmlAttr.style "height" "0.6em"
                , HtmlAttr.style "min-width" "90px"
                , HtmlAttr.style "max-width" "180px"
                , HtmlAttr.style "background-color" "#eef2f7"
                , HtmlAttr.style "border-radius" "2px"
                , HtmlAttr.style "overflow" "hidden"
                ]
                [ Html.div
                    [ HtmlAttr.style "height" "100%"
                    , HtmlAttr.style "width" (percentWidth row.count assignedTotal)
                    , HtmlAttr.style "background-color" (color row.category)
                    ]
                    []
                ]
            ]
        , Html.td [ HtmlAttr.style "text-align" "right" ]
            [ Html.text (if unassigned then "-" else percentageText row.count assignedTotal) ]
        ]
