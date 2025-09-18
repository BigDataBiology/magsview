module Pages.Static_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route { static : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "About"
            , body = [mdToHtml (content route.params.static)]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})


content : String -> String
content key = case key of
    "about" ->
        contentAbout
    "other" ->
        contentOther
    "manuscript" ->
        contentManuscript
    _ ->
        contentOther

contentAbout : String
contentAbout = """
## Shanghai dog gut microbiome

This was a project led by _Anna Cuscó_ (Fudan University) in the [Big Data Biology Lab](https://big-data-biology.org) led by _Luis Pedro Coelho_. The project is currently being finalized for publication. The data will also be made available at a suitable repository.

Please contact us if you are interested in the data: [anna@big-data-biology.org](mailto:anna@big-data-biology.org) or [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.rorg).
"""

contentOther : String
contentOther = """
## Shanghai dog gut microbiome



Please contact us if you are interested in the data: [anna@big-data-biology.org](mailto:anna@big-data-biology.org) or [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.rorg).

### MAG catalogue

The MAG catalogue includes:

1. The MAGs themselves, in FASTA format.
2. Eggog-mapper annotations for the MAGs.
3. RGI annotations for the MAGs (Antimicrobial Resistance Genes).
4. A table with the MAGs, their size, completeness, contamination, and other statistics.


### Extra-chromosomal elements

We provide a catalogue of extra-chromosomal elements, including plasmids.

1. The sequences themselves, in FASTA format.
2. A table with information including putative host prediction, element type classification (plasmid, phage, or unclassified).
3. Eggog-mapper annotations for the sequences.


### Gene catalogue

The gene catalogue includes three levels of redudancy removal:

1. The initial set of open reading frames (ORFs) predicted by Prodigal.
2. The set of ORFs after clustering at 100% nucleotide identity.
3. The set of ORFs after clustering at 95% nucleotide identity (unigenes).

As a resource for download, we provide:

1. The genes themselves, in FASTA format; including both nucleotides and amino acids versions.
2. Eggog-mapper annotations for the genes.
3. The clustering table.

### Small protein catalogue

Unlike the gene catalogue, which is completely _denovo_, the small protein
catalogue is based on mapping to the [Global Microbial Smorf
Catalogue](https://doi.org/10.1038/s41467-024-51894-6).

As a resource for download, we provide:

1. The small proteins themselves, in FASTA format, both 100% and 90% amino acid identity.
2. Clustering tables for the small proteins, at 100% and 90% amino acid identity.

### Other tables

Other tables, including contextual data tables, will be available as
Supplementary Tables in the manuscript.

"""

contentManuscript : String
contentManuscript = """
## Manuscript

> _Capturing global pet dog gut microbial diversity and hundreds of
> near-finished bacterial genomes by using long-read metagenomics in a Shanghai
> cohort_ by Anna Cuscó, Yiqian Duan, Fernando Gil, Alexei Chklovski, Nithya
> Kruthi, Shaojun Pan, Sofia Forslund, Susanne Lau, Ulrike  Löber, Xing-Ming
> Zhao, and Luis Pedro Coelho (bioRxiv PREPRINT 2025,
> [DOI:10.1101/2025.09.17.676595](https://doi.org/10.1101/2025.09.17.676595))

"""

