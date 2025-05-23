module GenomeStats exposing (chartNrContigs,
                            chartQualitySummary,
                            chartQualityScatter,
                            Quality, magQuality, qualityString,
                            taxonomyLast,
                            printableTaxonomy)


import Html
import Html.Attributes as HtmlAttr
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import DataModel exposing (MAG)


type Quality
    = High
    | Medium
magQuality : MAG -> Quality
magQuality mag =
    if mag.completeness > 90.0 && mag.contamination < 5.0 then
        High
    else Medium

qualityString : Quality -> String
qualityString quality =
    case quality of
        High ->
            "High"

        Medium ->
            "Medium"


chartNrContigs sel =
    let
        groups = sel
            |> List.map .nrContigs
            |> List.map (\x ->
                    if x < 2
                        then String.fromInt x
                    else if x < 6
                        then "2-5"
                    else if x < 11
                        then "6-10"
                    else if x < 21
                        then "11-20"
                    else if x < 51
                        then "21-50"
                    else if x < 100
                        then "50-99"
                    else "100+")
        data =
            ["1", "2-5", "6-10", "11-20", "21-50", "51-99", "100+"]
                |> List.map (\x -> { c = (groups |> List.filter ((==) x) |> List.length |> toFloat)
                                    , label = x })

    in
        C.chart
        [
        ]
        [ C.yLabels [ CA.withGrid ]
        , C.binLabels .label [ CA.moveDown 14, CA.fontSize 14 ]
        , C.labelAt .min CA.middle [ CA.moveLeft 65, CA.rotate 90 ]
              [ S.text "Number of MAGs" ]
        , C.labelAt CA.middle .min [ CA.moveDown 30 ]
              [ S.text "Number of contigs in genome" ]
        , C.bars []
            [ C.bar .c []
            ]
            data
        ]


chartQualitySummary sel =
    let
        qs = sel |> List.map (magQuality >> qualityString)
        high = List.filter ((==) "High") qs |> List.length |> toFloat
        medium = List.filter ((==) "Medium") qs |> List.length |> toFloat
    in
        C.chart
        [ ]
        [ C.yLabels [ CA.withGrid ]
        , C.binLabels .label [ CA.moveDown 20 ]
        , C.labelAt .min CA.middle [ CA.moveLeft 65, CA.rotate 90 ]
              [ S.text "Number of MAGs" ]
        , C.labelAt CA.middle .min [ CA.moveDown 30 ]
              [ S.text "Quality" ]

        , C.bars []
            [ C.bar .c []
            ]
            [ { c = high, label = "High"}
            , { c = medium, label = "Medium"}
            ]
        ]

chartQualityScatter onHover hovering sel =
  C.chart
    [ CE.onMouseMove onHover (CE.getNearest CI.dots)
    , CE.onMouseLeave (onHover [])
    ]
    [ C.xLabels [ CA.withGrid ]
    , C.yLabels [ CA.withGrid ]

    , C.labelAt .min CA.middle [ CA.moveLeft 35, CA.rotate 90 ]
          [ S.text "Contamination" ]
    , C.labelAt CA.middle .min [ CA.moveDown 30 ]
          [ S.text "Completeness" ]

    , C.each hovering <| \p item ->
        [ C.tooltip item [] []
            [
                let
                    m = CI.getData item
                in
                    Html.span [
                        HtmlAttr.style "font-weight"
                            (if m.isRepresentative
                                then "bold"
                                else "normal")
                        ] [
                       Html.text <| taxonomyLast m.taxonomy]
            ]
        ]

    , C.series .completeness
        [C.scatter .contamination []
              |> C.amongst hovering (\datum -> [ CA.highlight 0.5 ])
              |> C.variation
                    (\ix mag ->
                        [CA.color
                            (if mag.isRepresentative then
                                "green"
                            else
                                "orange")
                        ]
                    )

        ]
        -- draw the non-representative genomes first
        -- so that the representative genomes are on top
        (
            (sel |> List.filter (.isRepresentative >> not))
            ++
            (sel |> List.filter .isRepresentative)
        )
    ]

taxonomyLast t =
    String.split ";" t
        |> List.reverse
        |> List.filter (\s -> String.length s > 3)
        |> List.head
        |> Maybe.withDefault ""

printableTaxonomy =
    taxonomyLast >> String.dropLeft 3
