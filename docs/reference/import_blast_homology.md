# Import homology from a BLAST outfmt 6 file

Parses a BLAST tabular output file (outfmt 6) and creates a
`HomologyAnnotation` object mapping query-species genes to
reference-species genes. The best hit per query is selected by ranking
on one or more BLAST metrics.

## Usage

``` r
import_blast_homology(
  blast_file,
  reference_species,
  query_species,
  name = NULL,
  outfmt = paste("6 qseqid sseqid pident length mismatch",
    "gapopen qstart qend sstart send evalue", "bitscore"),
  rank_by = "bitscore",
  gene_id_map = NULL,
  strip_prefix = "^(transcript:|cds:|gene:)",
  strip_suffix = "(\\.t\\d+|-T\\d+)$",
  metadata = list()
)
```

## Arguments

- blast_file:

  Path to the BLAST outfmt 6 file.

- reference_species:

  Name of the reference (center) species.

- query_species:

  Name of the query species whose proteins were BLASTed.

- name:

  Optional label for the homology annotation. Defaults to the blast file
  stem.

- outfmt:

  The BLAST `-outfmt` column specification as a single string, e.g.
  `"6 qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore"`.
  The leading `"6 "` is stripped; the remaining tokens become the column
  names of the parsed table. This must match the columns actually
  written by BLAST.

- rank_by:

  One or more column names used to rank hits before deduplication. For
  `"evalue"` the sort is ascending (lower is better); all other columns
  are descending (higher is better). When multiple columns are given the
  first is the primary key. Defaults to `"bitscore"`.

- gene_id_map:

  Optional file path to a WormBase-style gene ID mapping (e.g.
  `c_elegans.PRJNA13758.WS285.geneIDs.txt`) or a named character vector
  mapping locus tags to gene names. When supplied, the `reference_gene`
  column is translated from locus tags (e.g. `"B0250.1"`) to gene names
  (e.g. `"calf-1"`). Isoform suffixes are stripped before lookup when an
  exact match is not found.

- strip_prefix:

  Regular expression matching prefixes to strip from query IDs. Defaults
  to `"^(transcript:|cds:|gene:)"`.

- strip_suffix:

  Regular expression matching suffixes to strip from query IDs. Defaults
  to `"(\\.t\\d+|-T\\d+)$"` (transcript isoform numbers, covering both
  `.t1` and Funannotate-style `-T1` conventions).

- metadata:

  Optional metadata list.

## Value

A `HomologyAnnotation` object.

## Details

The `outfmt` string is the exact argument passed to `blastp -outfmt`.
All columns declared in `outfmt` must be present in the file; extra
columns are ignored, and lines with fewer fields than declared are
discarded.

The `rank_by` parameter controls which BLAST metric(s) determine the
best hit kept per query. Common choices:

- `"bitscore"` — highest bitscore (default)

- `"pident"` — highest percent identity

- `"evalue"` — lowest e-value

- `c("pident", "evalue")` — highest identity, then lowest e-value on
  ties
