# Inject homology reference columns into a geom data frame

Adds `reference_gene` and `reference_gene_name` columns by matching
annotation feature IDs against `HomologyAnnotation` query genes.
Matching is done per-track using symmetric normalization of both
annotation-side IDs (gene_id, gene_name, transcript_id, Parent, ID) and
homology-side `query_gene` values.

## Usage

``` r
.inject_homology_columns(df, homology_list)
```

## Arguments

- df:

  A data frame produced by a `syn_*_df()` function. Must contain at
  least a `track` column and one or more of `gene_id`, `gene_name`,
  `transcript_id`, `Parent`, `ID`.

- homology_list:

  A named list of `HomologyAnnotation` objects, typically from
  `homology_annotations(synspecies)`.

## Value

The input data frame with additional `reference_gene`,
`reference_gene_name`, `homology_hit`, `homology_query_hit`,
`homology_reference_hit`, and `is_homology_reference_track` columns.

## Details

When no homology is available for a track, or when a feature has no
match, both new columns fall back to the feature's original gene_name or
gene_id, while `homology_hit` and `homology_query_hit` record whether a
real query-side homology table match was found. Reference-track rows
that match visible reference genes are marked with
`homology_reference_hit`.
