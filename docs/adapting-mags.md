# Adapting MAGs View to a different MAG catalog

The app ships with a baked-in set of MAG metadata (the table on the Genomes page) and
per-genome detail JSON files (used on each genome detail page). Gene annotations and
genome sequences are fetched at runtime from a remote server. To swap in another
catalog, update all three pieces so they stay in sync.

## 1) Prepare the MAG metadata CSV

The genome table is powered by a CSV string embedded in `src/Data/Blobs/MAGs.elm`.
This module is generated from `data/magsdata.csv` by `scripts/generate-mags-blob.py`;
do not edit it by hand.

`data/magsdata.csv` is the catalogue table, with one row per MAG and the following
header row:

```
Bin ID,Representative,Completeness,Contamination,Genome Size,Quality,Nr contigs,Classification,GTDBtk fastani Ref,16S rRNA,23S rRNA,5S rRNA,16S partial,23S partial,5S partial,Unique tRNAs,Total tRNAs,MIMAG,Sample,Original ID
```

The columns used by the generator are:

| Column | Used as |
| --- | --- |
| `Bin ID` | MAG ID (a `.fna.gz` suffix is stripped, e.g., `SHD1_0222.fna.gz` → `SHD1_0222`) |
| `Sample` | Sample the MAG was recovered from |
| `Classification` | Taxonomy, as a GTDB-style string (`d__Bacteria;p__...;s__...`) |
| `Completeness`, `Contamination` | Quality estimates (numbers) |
| `Genome Size`, `Nr contigs` | Integers |
| `16S rRNA`, `16S partial`, `23S rRNA`, `23S partial`, `5S rRNA`, `5S partial` | rRNA counts; complete and partial copies are added together. May be written as floats (`6.0`) or left empty (counted as 0) |
| `Unique tRNAs` | Number of tRNAs (integer) |
| `Representative` | Must be `Yes` or `No` |

The other columns are ignored. The binning tool and assembly method are constants
(`BINNING_TOOL` and `ASSEMBLY_METHOD`) at the top of `scripts/generate-mags-blob.py`;
change them if your catalogue was built differently. MAG IDs must be unique.

Once the CSV is in place, regenerate the Elm module:

```bash
scripts/generate-mags-blob.py              # rewrite src/Data/Blobs/MAGs.elm
scripts/generate-mags-blob.py --check      # only check that it is up to date
scripts/generate-mags-blob.py other.csv    # use a CSV in another location
```

`LoadData.loadData` parses the embedded CSV at startup.

## 2) Add per-genome JSON files

Each genome detail page fetches `/genome-data/<MAG_ID>.json` from the `static/genome-data`
folder. For every MAG in the CSV, create a matching JSON file named
`<MAG_ID>.json` with this structure:

```json
{
  "16S": [
    { "Seq": "<16S_sequence>", "OTU": "<otu_metadata>" }
  ],
  "ARGs": [
    {
      "Sequence": "<amino_acid_sequence>",
      "ArgName": "<ARG_name>",
      "ARO": "<ARO_identifier>",
      "Cut_Off": "<e.g., Perfect/Strict>",
      "Identity": <float>,
      "Coverage": <float>,
      "InResfinder": <true|false>,
      "Drug Class": "<drug_class_label>"
    }
  ]
}
```

You can include multiple entries in the `16S` and `ARGs` arrays. The JSON can be
compact or pretty-printed: Elm's decoder only depends on the field names and types.

## 3) Point to the remote data

Dataset naming (`datasetName`, `datasetTag`, `datasetSlug`) and the URLs of remotely
hosted files are set in `src/Data/Info.elm`. The app fetches, for each MAG:

- the genome FASTA (`mkFASTALink`), used to extract gene sequences in the browser;
- the eggNOG-mapper summary TSV (`mkEMapperSummaryLink`), used for the genome map and
  gene details (see [emapper-summary-tsv.md](emapper-summary-tsv.md) for the format);

and links to the full eggNOG-mapper annotations (`mkENOGLink`) and barrnap rRNA
predictions (`mkBarrnapLink`) for download. Update `baseURL` and these builders to
match your server layout, and update the README text and citation in `mkReadmeFile`,
which is included in bulk downloads.

The hidden `/healthcheck` page checks whether these remote resources are available.

## 4) Verify

Check that the metadata and the JSON files are consistent:

```bash
scripts/check-mags-data.py
```

This checks that the embedded CSV has the columns and types the Elm decoder expects,
that every MAG has a JSON file (and vice versa), that every JSON file has the expected
structure, and that the number of 16S sequences in each JSON file matches the 16S rRNA
count in the CSV.

Then, check the site itself:

1. Run `npx elm-land server` to rebuild the site.
2. Visit `/genomes` and `/taxonomy`, and click into a genome to make sure the metadata,
   genome map, and ARG/16S sections render without errors.
3. Visit `/healthcheck` to confirm that the remote files are reachable.
