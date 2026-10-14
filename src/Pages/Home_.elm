module Pages.Home_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "Shanghai dog gut MAGs"
            , body = [mdToHtml content]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})

content : String
content = """
## Shanghai dog gut microbiome

> _Capturing global pet dog gut microbial diversity and hundreds of
> near-finished bacterial genomes by using long-read metagenomics in a Shanghai
> cohort_ by Anna Cuscó, Yiqian Duan, Fernando Gil, Alexei Chklovski, Nithya
> Kruthi, Shaojun Pan, Sofia Forslund, Susanne Lau, Ulrike  Löber, Xing-Ming
> Zhao, and Luis Pedro Coelho (bioRxiv PREPRINT 2025,
> [DOI:10.1101/2025.09.17.676595](https://doi.org/10.1101/2025.09.17.676595))

Dogs are part of the family, and learning about their gut microbes can tell us
a lot about both their health and the household they live in. Most studies of
the dog gut microbiome so far have used short-read sequencing, which breaks
up genomes and misses important pieces like mobile elements, resistance genes,
and ribosomal genes.

![Shanghai dog](/images/Fig1a.svg)

For this project, we used deep long-read sequencing (ONT), polished with
short reads (Illumina) on stool samples from 51 pet dogs in Shanghai. This gave
us **2,676 high-quality genomes** from **320 different bacterial species**. In
total, 72% of them are close to finished quality, _often better than the
available reference genomes_.

![Shanghai dog](/images/Fig1e.svg)

When we compared our data to other dog microbiome datasets from around the
world, we found strong overlap (with over 90% of reads mapping to our genomes),
showing that what we see in _Shanghai dogs reflects pet dogs globally_.

In addition to the genomes, we recovered circular extrachromosomal elements and
built gene catalogs (including a catalog of putative small proteins).

### The catalogue at a glance

| | |
|---|---|
| **Cohort** | 51 pet dogs living in Shanghai households (52 samples, including one biological replicate) |
| **Sequencing** | At least 20 Gbp of Nanopore and 20 Gbp of Illumina data per sample (about 1,600 Gbp and 1,000 Gbp in total) |
| **MAGs** | 2,676: 1,928 near-finished, 16 high-quality and 732 medium-quality |
| **Species** | 320 species-level clusters (95% ANI), 102 of which are not in GTDB (r214) |
| **Circular extrachromosomal elements** | 185 (58 plasmids, 30 viruses and 97 uncategorized) |
| **Gene catalogue** | 1,470,802 non-redundant genes (95% nucleotide identity) |
| **Small protein catalogue** | 403,491 non-redundant small ORFs (273,065 clusters at 90% amino acid identity) |

_Near-finished_ MAGs meet every MIMAG high-quality criterion: over 90%
completeness, under 5% contamination, the 5S, 16S and 23S rRNA genes, and at
least 18 of the 20 standard tRNAs. _High-quality_ MAGs meet the completeness
and contamination thresholds but miss at least one of the other criteria, and
_medium-quality_ MAGs have over 50% completeness and under 10% contamination.
In this site's genome table, "High" covers both near-finished and high-quality
MAGs. Over 80% of the 320 species have a near-finished MAG as their
representative genome.

### Highlights from the study

- **Better than the reference genomes.** For the 156 species whose public
  reference genome is also high-quality, our MAGs were significantly more
  contiguous, carried more full-length rRNA genes and unique tRNAs, and
  captured more of the mobilome (COG category X).
- **New species.** About a third of the species (102) are not in GTDB r214.
  _CAG-269_ contributes the most (10 new species), followed by
  _Dysosmobacter_, _Collinsella_ and _Blautia A_ (four each).
- **A core of shared species.** Four Lachnospiraceae species were found in
  almost every dog: _Blautia hansenii_, _Enterocloster_ sp001517625,
  _Faecalimonas umbilicata_ and _Ruminococcus B gnavus_.
- **Resistance genes on mobile elements.** We found 253 unique antibiotic
  resistance genes, most often for tetracyclines, aminoglycosides and
  lincosamides. Ten circular elements carry resistance genes. These include
  two elements with the β-lactamase OXA-85, which had previously been reported
  only on bacterial chromosomes, and a putative _E. coli_ plasmid carrying
  TEM-1.
- **Sharing within households.** Of the 51 dogs, 19 came from 8 multi-dog
  households. Dogs in the same household shared more bacterial strains and
  extrachromosomal elements than dogs from different households.
- **Where dogs live matters more than geography.** Pet dogs from China, Germany,
  South Africa and the USA have similar gut microbiomes. Dogs living in
  colonies (such as research facilities) form a distinct group, with higher
  diversity and more _Bifidobacterium globosum_ and _Limosilactobacillus
  reuteri_ (possibly reflecting probiotic use). This
  matches the read mapping rates to our catalogue: a median of over 90% for
  pet dogs, over 86% for colony dogs and over 75% for shelter or free-roaming
  dogs.

### Using this site

- The [taxonomy browser](/taxonomy) organizes the MAGs by their GTDB
  classification.
- The [genome table](/genomes) lists every MAG with its quality statistics. It
  can be filtered and downloaded.
- Each genome page shows the gene map with eggNOG-mapper annotations, rRNA
  genes, 16S rRNA links to [MicrobeAtlas](https://microbeatlas.org/) (to
  connect the genome with 16S amplicon studies), and antibiotic resistance
  genes predicted with RGI.
- [Other data](/other) describes the extrachromosomal element, gene and small
  protein catalogues and where to download them.
"""

