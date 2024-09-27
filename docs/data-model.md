# Data Model

There are three main data types in the system:

- `Sample`
- `MAG`
- `Feature`

And a relationship type:

- `FeatureInGenome`

## Sample

Contains the following fields:

- `id` (string): Unique identifier for the sample.
- `external_id` (string): External identifier for the sample (e.g., SRA, ENA, etc.).
- `latitude` (float): Latitude of the sample.
- `longitude` (float): Longitude of the sample.
- `collection_date` (date): Date of collection.
- `microontology` (string): Biome of the sample. Using micro-ontology is mandatory.
- `biome_expanded` (string): Expanded biome of the sample.
- `comment` (string): Additional information about the sample.

## MAG

- `id` (string): Unique identifier for the MAG.
- `samples_id` (list of string): Identifier of the sample(s) the MAG was recovered from.
- `taxonomy` (string): Taxonomy of the MAG.
- `completeness` (float): Completeness of the MAG.
- `contamination` (float): Contamination of the MAG.
- `#16s_rrna` (int): Number of 16S rRNA genes detected in the MAG.
- `#5s_rrna` (int): Number of 5S rRNA genes detected in the MAG.
- `#23s_rrna` (int): Number of 23S rRNA genes detected in the MAG.
- `#trna` (int): Number of tRNA genes in the MAG.
- `nr_contigs` (int): Number of contigs in the MAG.
- `nr_genes` (int): Number of genes in the MAG.
- `is_representative` (bool): Whether the MAG is representative for its species.
- `binning_tool` (string): Tool used for binning.
- `assembly_method` (string): Method used for assembly.
- `comment` (string): Additional information about the MAG.
- `file_size` (int): Size of the MAG file.
- `file_sha256` (string): SHA256 checksum of the MAG file.

## Feature

- `id` (string): Unique identifier for the feature.
- `name` (string): Name of the feature.
- `vocabulary` (string): Type of the feature (e.g., KEGG, COG, etc.).

## FeatureInGenome

- `feature_id` (string): Unique identifier for the feature in the genome.
- `genome_id` (string): Unique identifier for the genome.
- `contig` (string): Contig of the feature in the genome.
- `start` (int): Start position of the feature in the genome.
- `end` (int): End position of the feature in the genome.
- `score` (float): Score of the feature in the genome.
- `comment` (string): Additional information about the feature in the genome.
