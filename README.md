# MAGs View

A static web interface to browse and explore a catalogue of metagenome-assembled
genomes (MAGs): their quality, taxonomic classification, and functional
annotations.

## Examples

MAGs View powers the following catalogues:

- **[Shanghai Dog Gut MAGs](https://sh-dog-mags.big-data-biology.org/)**: MAGs
  recovered from long-read metagenomes of pet dogs in Shanghai. Described in
  Cuscó _et al._, [Capturing global pet dog gut microbial diversity and hundreds
  of near-finished bacterial genomes by using long-read metagenomics in a
  Shanghai cohort](https://www.biorxiv.org/content/10.1101/2025.09.17.676595v1)
  (bioRxiv 2025).
- **[Urban Soil MAGs](https://urban-soil-mags.big-data-biology.org/)**: MAGs
  recovered from long-read metagenomes of urban green-space soils in Shanghai
  and Nanjing. Described in Duan _et al._, [Long-read metagenomic sequencing
  reveals novel lineages and functional diversity in urban soil
  microbiome](https://www.biorxiv.org/content/10.64898/2026.03.20.713087v1)
  (bioRxiv 2026).

## Features

- **Genome table** (`/genomes`): filterable and sortable list of all MAGs with
  quality metrics (completeness, contamination, genome size, rRNA/tRNA counts).
- **Taxonomy browser** (`/taxonomy`): explore the catalogue by taxonomic rank.
- **Genome detail pages** (`/genome/<MAG_ID>`): per-MAG summary, genome map with
  eggNOG-mapper annotations, 16S rRNA matches, and antibiotic resistance genes
  (ARGs).
- **Sequence access**: click a gene to view its nucleotide or protein sequence,
  or download gene sequences as FASTA. Genome FASTA files are fetched and
  decompressed in the browser on demand.
- **Downloads**: links to the raw genome and annotation files for each MAG.

## Local development

The app is written in [Elm](https://elm-lang.org/) using
[Elm Land](https://elm.land/).

```bash
# Requires Node.js v18+ (https://nodejs.org)
npx elm-land server
```

To run the unit tests for the data decoders:

```bash
npx elm-test@0.19.1-revision15
```

To produce a production build (output in `result/`):

```bash
nix build .
```

## Project layout

| Path | Contents |
| --- | --- |
| `src/Pages/` | Route-backed pages (Elm Land conventions) |
| `src/Layouts/` | Shared page layout |
| `src/Data/Info.elm` | Dataset name and URL builders for remote data |
| `src/Data/Blobs/MAGs.elm` | MAG metadata baked into the app (generated, do not edit) |
| `src/EMapper.elm`, `src/Barrnap.elm`, `src/GenomeData.elm`, `src/LoadData.elm` | Parsers for the annotation files, per-genome JSON, and MAG metadata |
| `src/interop.js` | JavaScript side of the Elm ports (FASTA fetching, sequence extraction) |
| `data/magsdata.csv` | Source table for the MAG metadata |
| `static/genome-data/` | Per-genome JSON files (16S rRNA matches, ARGs) |
| `tests/` | Unit tests (elm-test), one module per source module |
| `scripts/` | Scripts to generate and check the MAG metadata |
| `docs/` | Data format documentation |

## Using a different MAG catalogue

The MAG metadata is generated from `data/magsdata.csv`:

```bash
scripts/generate-mags-blob.py   # regenerate src/Data/Blobs/MAGs.elm
scripts/check-mags-data.py      # check it is in sync with static/genome-data/*.json
```

Dataset naming and the URLs of remotely hosted files (genomes, annotations) are
set in `src/Data/Info.elm`.

See [docs/adapting-mags.md](docs/adapting-mags.md) for details on swapping in your
own MAG metadata and per-genome JSON files, and
[docs/emapper-summary-tsv.md](docs/emapper-summary-tsv.md) for the format of the
gene annotation files.

## License

This project is licensed under the MIT License; see [LICENSE.MIT](LICENSE.MIT)
for details.
