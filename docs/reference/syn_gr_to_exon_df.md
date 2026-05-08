# Convert genomic features to a `geom_exon()` data frame

Turns a `GRanges` annotation subset into the rectangular feature table
used by
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).
The returned data always includes a canonical identifier set for
aesthetic mappings:

## Usage

``` r
syn_gr_to_exon_df(feature_gr, track, annotation_type = "exon")
```

## Arguments

- feature_gr:

  A `GRanges` object containing exon-like annotation features.

- track:

  Track label written into the output table.

- annotation_type:

  Feature type to keep. Defaults to `"exon"`. When `"exon"`, CDS rows
  are used only as a fallback for transcripts that do not already have
  explicit exon records.

## Value

A `data.frame` ready for
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
with positional columns plus canonical identifier columns such as
`transcript_id`, `gene_id`, and `gene_name`.

## Details

- `transcript_id`: normalized transcript-level identifier

- `gene_id`: normalized gene-level identifier

- `gene_name`: display-friendly gene label

The existing `transcripts` column is retained because ggexon uses it
internally for grouping and track layout.
