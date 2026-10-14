module GenomeMapTests exposing (suite)

import EMapper exposing (EMapperGene, GeneType(..))
import Expect
import GenomeMap
import Test exposing (Test, describe, test)


gene : String -> String -> Int -> Int -> EMapperGene
gene seqid contig start end =
    { seqid = seqid
    , contig = contig
    , start = start
    , end = end
    , strand = "+"
    , cogCategory = ""
    , preferredName = ""
    , keggKo = []
    , keggModule = []
    , geneType = ProteinCoding
    }


genes : List EMapperGene
genes =
    [ gene "a2" "a" 500 900
    , gene "b1" "b" 1 5000
    , gene "a1" "a" 1 400
    ]


zoomed : Float -> Float -> GenomeMap.State
zoomed zoom offset =
    let
        s = GenomeMap.init
    in
    { s | zoom = zoom, offset = offset }


suite : Test
suite =
    describe "GenomeMap"
        [ test "groupByContig puts the longest contig first" <|
            \_ ->
                GenomeMap.groupByContig genes
                    |> List.map (\c -> ( c.name, c.maxPos, List.map .seqid c.genes ))
                    |> Expect.equal [ ( "b", 5000, [ "b1" ] ), ( "a", 900, [ "a2", "a1" ] ) ]
        , test "geneOrder goes contig by contig, then by position" <|
            \_ ->
                GenomeMap.geneOrder (GenomeMap.groupByContig genes)
                    |> List.map .seqid
                    |> Expect.equal [ "b1", "a1", "a2" ]
        , test "maxPos of no contigs is 1 (to avoid dividing by zero)" <|
            \_ ->
                GenomeMap.maxPos [] |> Expect.equal 1
        , test "zooming in and out around the same point is a no-op" <|
            \_ ->
                zoomed 4 0.3
                    |> GenomeMap.zoomAround 2 0.25
                    |> GenomeMap.zoomAround 0.5 0.25
                    |> (\s -> ( s.zoom, round (s.offset * 1000) ))
                    |> Expect.equal ( 4, 300 )
        , test "zoom is clamped to at least 1" <|
            \_ ->
                GenomeMap.zoomAround 0.1 0.5 (zoomed 2 0.5)
                    |> (\s -> ( s.zoom, s.offset ))
                    |> Expect.equal ( 1, 0 )
        , test "panning stops at the right end" <|
            \_ ->
                GenomeMap.panBy 10 (zoomed 2 0.5)
                    |> .offset
                    |> Expect.equal 1
        , test "visibleRange at 4x, scrolled to the end" <|
            \_ ->
                GenomeMap.visibleRange 1000 (zoomed 4 1)
                    |> Expect.equal ( 750, 1000 )
        , test "scrollToGene leaves a visible gene alone" <|
            \_ ->
                GenomeMap.scrollToGene (GenomeMap.groupByContig genes) (gene "b1" "b" 100 200) (zoomed 4 0)
                    |> .offset
                    |> Expect.equal 0
        , test "scrollToGene centres a gene that is out of view" <|
            \_ ->
                GenomeMap.scrollToGene (GenomeMap.groupByContig genes) (gene "x" "b" 2400 2600) (zoomed 4 0)
                    |> GenomeMap.visibleRange 5000
                    |> Expect.equal ( 1875, 3125 )
        , test "a click is only swallowed right after a drag" <|
            \_ ->
                GenomeMap.consumeDragClick GenomeMap.init
                    |> Expect.equal Nothing
        ]
