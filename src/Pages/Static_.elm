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
            { title = "Shanghai dog gut MAGs: " ++ title route.params.static
            , body = [mdToHtml (content route.params.static)]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})


title : String -> String
title key = case key of
    "about" ->
        "About"
    "manuscript" ->
        "Manuscript"
    _ ->
        "Other data"

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

This project was led by _Anna Cuscó_ (Fudan University) in the [Big Data
Biology Lab](https://big-data-biology.org), led by _Luis Pedro Coelho_. It is
described in [our preprint](/manuscript) (Cuscó et al., bioRxiv 2025).

### The cohort

We collected a fresh fecal sample from 51 pet dogs living in Shanghai (China).
The dogs share an urban, indoor lifestyle but differ in age, size, breed, diet
and habits. Their owners answered a questionnaire covering over 50 lifestyle,
health and diet variables. Nineteen of the dogs came from eight households with
more than one sampled dog.

### How the genomes were built

1. **Sequencing.** Each sample was sequenced to at least 20 Gbp with Oxford
   Nanopore (R10.4.1 flow cells on a PromethION, basecalled with Guppy's
   high-accuracy model) and at least 20 Gbp with Illumina (NovaSeq 6000,
   2×150 bp). Some samples have up to 65 Gbp of long reads.
2. **Assembly.** Long reads were assembled with MetaFlye. To recover species
   missed in the full-depth assembly, we also assembled 20 Gbp and 10 Gbp
   subsamples.
3. **Polishing.** Assemblies were polished with Medaka (long reads), then
   Polypolish and Polca (short reads). Polishing raised 177 MAGs from medium
   to high quality, with the biggest gains for genomes below 40× coverage.
4. **Binning.** Contigs were binned with
   [SemiBin2](https://doi.org/10.1093/bioinformatics/btad209) (single-sample
   binning with short reads, single-sample binning with long reads, and
   multi-sample binning), and the results were combined with DAS Tool.
5. **Quality control and annotation.** Completeness and contamination were
   estimated with CheckM2 and taxonomy assigned with GTDB-Tk (GTDB r214).
   rRNA genes were found with barrnap (and RNAmmer for missing 5S genes) and
   tRNAs with tRNAscan. Species-level representatives were chosen with dRep at
   95% average nucleotide identity.
6. **Functional annotation.** Genes were predicted with Prodigal and annotated
   with eggNOG-mapper. Antibiotic resistance genes were predicted with RGI
   (CARD), keeping Strict and Perfect hits with at least 90% identity and 90%
   coverage.

The full analysis code is on GitHub:
[BigDataBiology/ShanghaiDogs](https://github.com/BigDataBiology/ShanghaiDogs).

### Data availability

- Raw reads and the MAG catalogue: European Nucleotide Archive, BioProject
  [PRJEB85799](https://www.ebi.ac.uk/ena/browser/view/PRJEB85799).
- MAG annotations, resistance gene predictions, and the extrachromosomal
  element, gene and small protein catalogues: Zenodo,
  [doi:10.5281/zenodo.16356977](https://doi.org/10.5281/zenodo.16356977). See
  [Other data](/other) for details.
- Short-read data for the 15-dog Berlin cohort used for comparison: BioProject
  [PRJNA881055](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA881055).

### Contact

Please contact us with any questions about the data:
[anna@big-data-biology.org](mailto:anna@big-data-biology.org) or
[luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.org).
"""

contentOther : String
contentOther = """
## Shanghai dog gut microbiome: other data

Apart from the MAGs, which you can browse on this site, the study produced
several other resources. They can be downloaded from
[Zenodo](https://doi.org/10.5281/zenodo.16356977) (file names are given
below). Raw reads are at the European Nucleotide Archive under BioProject
[PRJEB85799](https://www.ebi.ac.uk/ena/browser/view/PRJEB85799).

### MAG catalogue

The catalogue contains 2,676 MAGs from 320 species. It includes:

1. The MAGs themselves, in FASTA format (from the [genome table](/genomes), each
   genome page, or ENA).
2. eggNOG-mapper annotations (`SHD1_eggNOG_MAG_annotations.tar.gz`).
3. The extracted rRNA genes, from barrnap (`SHD1_rRNAs_fasta_barrnap.tar.gz`).
4. RGI annotations for antibiotic resistance genes, for both the MAGs
   (`SHD1_ARGs_MAGs.csv.gz`) and all assembled contigs
   (`SHD1_ARGs_contigs.csv.gz`). Of the 253 unique resistance genes, 43 were
   found only on contigs outside the MAGs.
5. A table with each MAG's size, completeness, contamination and other
   statistics (Supplementary Table S3 in the manuscript; also available from
   the genome table).

### Extrachromosomal elements

We assembled 185 non-redundant, circular, single-contig extrachromosomal
elements: 58 plasmids (4,333 to 154,111 bp, median 30,562 bp), 30 viruses and
97 uncategorized elements. Ten of them carry antibiotic resistance genes. We
predicted a putative bacterial host for 52 of them (28%) and describe these
predictions in the manuscript.

1. The sequences themselves, in FASTA format (`SHD1_EC.fna.gz`).
2. A table with each element's sample, contig and category (plasmid, virus or
   uncategorized) (`SHD1_EC_props.tsv.gz`).

### Gene catalogue

Open reading frames (ORFs) were predicted with Prodigal. We provide three
levels of redundancy removal:

1. The initial set of ORFs (`SHD1_GC.ORF.fna.xz`).
2. ORFs clustered at 100% nucleotide identity (`SHD1_GC.100NT.fna.xz`).
3. ORFs clustered at 95% nucleotide identity, giving species-level unigenes
   (`SHD1_GC.95NT.fna.xz`, with proteins in `SHD1_GC.95NT.faa.xz`).

The final catalogue has 1,470,802 non-redundant genes. Of these, 98.7% are
complete genes and 73.5% are found in MAGs or extrachromosomal elements.

We also provide eggNOG-mapper annotations for the unigenes
(`SHD1_GC.95NT.emapper.annotations.gz`), the clustering table linking each ORF
to its 100% and 95% clusters (`SHD1_GC.clusters.tsv.xz`), and the sample,
contig and coordinates of each ORF (`SHD1_GC.ORF.orig.tsv.xz`).

### Small protein catalogue

Unlike the gene catalogue, which is entirely _de novo_, the small protein
catalogue is based on mapping to the [Global Microbial Smorf
Catalogue](https://doi.org/10.1038/s41467-024-51894-6) with GMSC-mapper. It has
403,491 non-redundant small ORFs (smORFs), which form 273,065 clusters at 90%
amino acid identity.

1. The small proteins themselves, in FASTA format, at 100% (`SHD1_SM.100AA.faa.gz`)
   and 90% (`SHD1_SM.90AA.faa.gz`) amino acid identity.
2. The clustering table from 100% to 90% identity (`SHD1_SM.clusters.tsv.gz`)
   and the sample, contig and coordinates of each small protein
   (`SHD1_SM.100AA_origins.tsv.gz`).

### Other tables

Other tables are available as Supplementary Tables in the
[manuscript](/manuscript). They include the dog metadata and questionnaire
answers, and curated metadata for public dog gut metagenomes.

Please contact us if you have questions about the data:
[anna@big-data-biology.org](mailto:anna@big-data-biology.org) or
[luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.org).
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

### Summary

Pet dogs are considered part of the family, and understanding their gut
microbiomes can provide insights into both animal and household health. Most
comprehensive studies, however, relied on short-read sequencing, resulting in
fragmented MAGs that miss mobile elements, antimicrobial-resistance genes, and
ribosomal genes. Here, we applied deep long-read metagenomics (polished with
short-reads) to fecal samples from 51 urban pet dogs in Shanghai, generating
2,676 MAGs—representing 320 bacterial species—, of which ∼72% achieved
near-finished quality, often improving on the corresponding reference public
genome. Comparisons with external datasets showed that our Shanghai-based MAG
catalog is representative of pet dogs worldwide (median read mapping of >90%).
Moreover, we recovered circular extrachromosomal elements, including those
linked to antimicrobial resistance, which were also detected in external dog
gut datasets. In conclusion, we provide a high-quality reference resource and
demonstrate the power of deep long-read metagenomics to resolve microbial
diversity in complex host-associated microbiomes.

### Citing this resource

If you use these data, please cite the manuscript above. The data are
available from the European Nucleotide Archive (BioProject
[PRJEB85799](https://www.ebi.ac.uk/ena/browser/view/PRJEB85799)) and
[Zenodo](https://doi.org/10.5281/zenodo.16356977), and the analysis code is on
[GitHub](https://github.com/BigDataBiology/ShanghaiDogs).
"""
