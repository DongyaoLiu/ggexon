# Normalize BLAST query IDs to gene-level identifiers

Strips common prefixes (e.g., `transcript:`, `cds:`) and transcript
isoform suffixes (e.g., `.t1`, `.t2`) so that BLAST query identifiers
can be matched against annotation gene IDs.

## Usage

``` r
.normalize_blast_query_id(
  x,
  strip_prefix = "^(transcript:|cds:|gene:)",
  strip_suffix = "(\\.t\\d+|-T\\d+)$"
)
```

## Arguments

- x:

  Character vector of query sequence identifiers.

- strip_prefix:

  Regular expression for prefixes to remove.

- strip_suffix:

  Regular expression for suffixes to remove.

## Value

A character vector of normalized gene-level identifiers.
