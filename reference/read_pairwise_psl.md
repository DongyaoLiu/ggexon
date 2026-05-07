# Read a PSL pairwise alignment into ggexon's internal link table

Parses a UCSC PSL file and returns the PAF-like table used internally by
`ggexon` for pairwise link dispatch. By default the parser keeps one row
per PSL record and normalizes the output columns to `qchr`, `qstart`,
`qend`, `tchr`, `tstart`, `tend`, `strand`, `nmatch`, `alen`, and
related fields expected by
[`pairwise_alignment_data()`](https://dongyaoliu.github.io/ggexon/reference/pairwise_alignment_data.md)
and
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).
When `more = TRUE`, each PSL record is expanded into one row per
ungapped block.

## Usage

``` r
read_pairwise_psl(
  path,
  query_individual = NULL,
  target_individual = NULL,
  more = FALSE
)
```

## Arguments

- path:

  Path to a PSL file.

- query_individual:

  Optional query-side individual identifier used to strip a species
  prefix from `qName` when inferring `qchr`.

- target_individual:

  Optional target-side individual identifier used to strip a species
  prefix from `tName` when inferring `tchr`.

- more:

  Logical; when `TRUE`, expand each PSL record into one row per ungapped
  alignment block using `blockSizes`, `qStarts`, `tStarts`, the
  query/target sequence lengths, and the PSL strand field to compute
  detailed block coordinates.

## Value

A PAF-like `data.frame`. When `more = TRUE`, the returned table includes
additional block-level columns such as `psl_row`, `block_index`,
`block_size`, `qstrand`, `tstrand`, and raw block starts.
