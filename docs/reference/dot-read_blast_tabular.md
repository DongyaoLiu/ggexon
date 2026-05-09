# Read a BLAST tabular file with named columns

Reads a tab-separated BLAST outfmt 6 file, assigns column names from the
parsed `outfmt` specification, and coerces numeric columns
automatically.

## Usage

``` r
.read_blast_tabular(blast_file, col_names)
```

## Arguments

- blast_file:

  Path to the BLAST tabular file.

- col_names:

  Character vector of column names, as returned by
  [`.parse_blast_outfmt()`](https://dongyaoliu.github.io/ggexon/reference/dot-parse_blast_outfmt.md).

## Value

A data frame with named columns.
